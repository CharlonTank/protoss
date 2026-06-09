"use strict";

const path = require("path");

const IDENTIFIER_RE = /[A-Za-z_][A-Za-z0-9_.]*/g;
const WORD_RE = /[A-Za-z_][A-Za-z0-9_.]*/;

// Primitives intégrées au kernel (aucune définition .protoss). Cmd/Ctrl+Click
// les renvoie vers les signatures documentées dans builtins.protoss.
const BUILTINS = new Set([
  "text",
  "image",
  "button",
  "input",
  "column",
  "row",
  "list",
  "when",
  "node",
  "attr",
  "on",
  "done",
  "bind",
  "AskHuman",
  "HttpGet",
  "LoadLocal",
  "SaveLocal",
  "ServerRequest"
]);

function stripLineComment(line) {
  let inString = false;
  let escaped = false;
  for (let i = 0; i < line.length; i += 1) {
    const ch = line[i];
    const next = line[i + 1];
    if (!inString && ch === ";") {
      return line.slice(0, i);
    }
    if (!inString && ch === "-" && next === "-") {
      return line.slice(0, i);
    }
    if (inString && ch === "\\" && !escaped) {
      escaped = true;
      continue;
    }
    if (ch === "\"" && !escaped) {
      inString = !inString;
    }
    escaped = false;
  }
  return line;
}

function addDefinition(defs, seen, name, line, character, kind) {
  if (seen.has(name)) {
    return;
  }
  seen.add(name);
  defs.push({ name, line, character, kind });
}

function scanDefinitions(text) {
  const defs = [];
  const seen = new Set();
  const lines = text.split(/\r?\n/);

  for (let lineNo = 0; lineNo < lines.length; lineNo += 1) {
    const raw = lines[lineNo];
    const line = stripLineComment(raw);
    const trimmed = line.trim();
    if (trimmed === "") {
      continue;
    }

    const sexpDef = line.match(/^\s*\((def|defpoly|defcap|defpolycap|defrec|defrecpoly)\s+([A-Za-z_][A-Za-z0-9_.]*)\b/);
    if (sexpDef) {
      addDefinition(defs, seen, sexpDef[2], lineNo, line.indexOf(sexpDef[2]), "function");
      continue;
    }

    const sexpType = line.match(/^\s*\((type|alias|record|variant)\s+([A-Za-z_][A-Za-z0-9_.]*)\b/);
    if (sexpType) {
      addDefinition(defs, seen, sexpType[2], lineNo, line.indexOf(sexpType[2]), "type");
      continue;
    }

    const elmTypeAlias = line.match(/^(\s*)type\s+alias\s+([A-Z][A-Za-z0-9_.]*)\b/);
    if (elmTypeAlias && elmTypeAlias[1].length === 0) {
      addDefinition(defs, seen, elmTypeAlias[2], lineNo, line.indexOf(elmTypeAlias[2]), "type");
      continue;
    }

    const elmType = line.match(/^(\s*)type\s+([A-Z][A-Za-z0-9_.]*)\b/);
    if (elmType && elmType[1].length === 0) {
      addDefinition(defs, seen, elmType[2], lineNo, line.indexOf(elmType[2]), "type");
      continue;
    }

    const elmSignature = line.match(/^(\s*)([A-Za-z_][A-Za-z0-9_.]*)\s*:/);
    if (elmSignature && elmSignature[1].length === 0) {
      addDefinition(defs, seen, elmSignature[2], lineNo, line.indexOf(elmSignature[2]), "function");
      continue;
    }

    const elmValue = line.match(/^(\s*)([A-Za-z_][A-Za-z0-9_.]*)\b[^=\n]*=/);
    if (elmValue && elmValue[1].length === 0) {
      addDefinition(defs, seen, elmValue[2], lineNo, line.indexOf(elmValue[2]), "function");
    }
  }

  return defs;
}

function findDefinitionInText(text, symbol) {
  return scanDefinitions(text).find((def) => def.name === symbol) || null;
}

function symbolAtTextOffset(lineText, character) {
  IDENTIFIER_RE.lastIndex = 0;
  let match;
  while ((match = IDENTIFIER_RE.exec(lineText)) !== null) {
    const start = match.index;
    const end = start + match[0].length;
    if (character >= start && character <= end) {
      return match[0];
    }
  }
  return null;
}

function activate(context) {
  const vscode = require("vscode");

  function locationFor(documentOrUri, def) {
    const uri = documentOrUri.uri || documentOrUri;
    const start = new vscode.Position(def.line, def.character);
    const end = new vscode.Position(def.line, def.character + def.name.length);
    return new vscode.Location(uri, new vscode.Range(start, end));
  }

  async function searchWorkspace(document, symbol, token) {
    const uris = await vscode.workspace.findFiles(
      "**/*.protoss",
      "**/{.protoss,_build,target,dist,node_modules}/**",
      5000,
      token
    );
    const current = document.uri.toString();
    uris.sort((a, b) => a.fsPath.localeCompare(b.fsPath));

    for (const uri of uris) {
      if (uri.toString() === current) {
        continue;
      }
      if (token && token.isCancellationRequested) {
        return null;
      }
      const candidate = await vscode.workspace.openTextDocument(uri);
      const def = findDefinitionInText(candidate.getText(), symbol);
      if (def) {
        return locationFor(uri, def);
      }
    }
    return null;
  }

  async function builtinDefinition(symbol) {
    if (!BUILTINS.has(symbol)) {
      return null;
    }
    const builtinsUri = vscode.Uri.file(path.join(__dirname, "builtins.protoss"));
    try {
      const doc = await vscode.workspace.openTextDocument(builtinsUri);
      const def = findDefinitionInText(doc.getText(), symbol);
      if (def) {
        return locationFor(builtinsUri, def);
      }
    } catch (err) {
      // builtins.protoss missing or unreadable — fall through to no result.
    }
    return null;
  }

  const provider = {
    async provideDefinition(document, position, token) {
      const range = document.getWordRangeAtPosition(position, WORD_RE);
      const symbol = range ? document.getText(range) : symbolAtTextOffset(document.lineAt(position.line).text, position.character);
      if (!symbol) {
        return null;
      }

      const local = findDefinitionInText(document.getText(), symbol);
      if (local) {
        return locationFor(document, local);
      }

      const workspaceHit = await searchWorkspace(document, symbol, token);
      if (workspaceHit) {
        return workspaceHit;
      }

      return builtinDefinition(symbol);
    }
  };

  context.subscriptions.push(
    vscode.languages.registerDefinitionProvider({ language: "protoss", scheme: "file" }, provider),
    vscode.languages.registerDefinitionProvider({ language: "protoss", scheme: "untitled" }, provider)
  );
}

function deactivate() {}

module.exports = {
  activate,
  deactivate,
  scanDefinitions,
  findDefinitionInText,
  stripLineComment,
  symbolAtTextOffset
};
