Protoss — Spécification de langage v0.1

Protoss est un langage auto-hébergé conçu pour être modifié principalement par agents IA via MCP, pas par édition manuelle de fichiers. Son unité fondamentale n’est pas le fichier texte, mais un graphe canonique typé, globalement hash-consé, où chaque expression, type, fonction, module, test, patch et artefact d’exécution possède une identité stable dérivée de son contenu. Cette spec reprend le modèle déjà posé : noyau total/normalisant, DAG Merkle global, mémoïsation adressée par contenu, effets médiés par un ledger, et patches MCP validés avant insertion dans l’univers canonique.

1. Vision

Protoss est un langage pour systèmes logiciels maintenus par humains et IA.

Ses principes fondateurs :

Le programme n’est pas du texte.
Le programme est un graphe canonique immutable.
Le texte est une vue humaine du graphe.
L’édition principale se fait par patches MCP structurés.
Les fonctions identiques sont physiquement unifiées par hash-consing global.
Les effets sont explicites, typés, rejouables et indexés par un monde versionné.
Le langage doit pouvoir s’auto-décrire, s’auto-modifier et s’auto-héberger.
L’IA n’est pas un plugin : elle est intégrée dans le modèle de développement, de test, de refactor et de migration.
2. Nom

Nom officiel : Protoss

Extensions proposées :

.pt       Protoss Human Syntax
.ptc      Protoss Canonical Text
.ptb      Protoss Binary Canonical
.ptpatch  Protoss Patch

La vraie source de vérité est .ptb ou son équivalent stocké dans le graphe global. Les fichiers .pt sont des projections lisibles, utiles pour revue, apprentissage, documentation et bootstrap.

3. Modèle mental

Dans un langage classique :

texte source -> AST -> typecheck -> compilation -> artefact

Dans Protoss :

patch MCP -> DAG typé canonique -> normalisation -> hash-consing global -> vues multiples -> artefacts dérivés

Le texte n’est jamais l’autorité finale.

Un humain peut écrire :

add : Int -> Int -> Int
add a b =
    a + b

Mais Protoss stocke plutôt une représentation canonique du genre :

Def {
  type = Pi Int (Pi Int Int)
  body = Lam Int (Lam Int (Prim.Add (Var 1) (Var 0)))
}

Puis calcule :

DefId = hash(canonicalBytes(type), canonicalBytes(body), constraints)

Deux définitions qui ont la même forme canonique deviennent la même définition logique.

4. Objectifs non négociables
4.1 Hash-consing global

Tout nœud canonique est interné globalement.

NodeId = H("protoss.node.v1", CanonicalBytes(Node))
DefId  = H("protoss.def.v1", NodeId(type), NodeId(body), constraints)

Si deux projets, deux agents ou deux utilisateurs produisent exactement la même fonction canonique, ils pointent vers le même objet.

Conséquence : Protoss est un langage où la déduplication, le cache, la provenance et la comparaison sémantique sont natifs.

4.2 Multi-syntaxe

Protoss possède au minimum deux syntaxes officielles.

Syntaxe humaine : Protoss/H

Lisible, inspirée d’Elm.

Caractéristiques :

- indentation significative
- fonctions pures par défaut
- ADT / union types
- records
- pattern matching
- pipes
- modules explicites
- pas de mutation implicite
- pas d’exception implicite
- effets typés

Exemple :

module Counter exposing (Model, Msg, init, update)

type alias Model =
    { count : Int }

type Msg
    = Increment
    | Decrement

init : Model
init =
    { count = 0 }

update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment ->
            { model | count = model.count + 1 }

        Decrement ->
            { model | count = model.count - 1 }
Syntaxe optimisée : Protoss/C

Machine-oriented, stable, compacte, non ambiguë.

Caractéristiques :

- pas de noms locaux sémantiques
- variables par indices de De Bruijn
- champs ordonnés canoniquement
- aucun sucre syntaxique
- aucune indentation significative
- aucune annotation non sémantique
- sérialisation déterministe

Exemple approximatif :

(def
  :id h:8f21...
  :type (pi Int (pi Int Int))
  :body
    (lam Int
      (lam Int
        (prim.add (var 1) (var 0)))))
Syntaxe binaire : Protoss/B

Format canonique réel pour hashing, stockage et transport.

.pt  -> vue humaine
.ptc -> vue canonique textuelle
.ptb -> canon binaire hashable

Règle stricte :

hash(parse(.pt)) == hash(parse(.ptc)) == hash(.ptb)

Si cette égalité ne tient pas, une des vues est invalide.

5. Source de vérité

La source de vérité est le Protoss Universe Root :

UniverseRoot = H(packages, defs, types, harnesses, policies, worldRefs)

Un projet Protoss n’est pas un dossier Git au sens classique. C’est une racine Merkle.

Project {
  root      : UniverseRoot
  exports   : Map Name DefId
  modules   : Map ModuleName ModuleId
  harnesses : Map DefId HarnessId
  policies  : PolicySet
}

Les noms sont des métadonnées. Les identités sont les hashes.

6. Noyau sémantique

Protoss/Core est un lambda-calcul typé total avec données algébriques, records, pattern matching, processus productifs et capacités.

Grammaire simplifiée :

Term
  = Var Index
  | Lam Type Term
  | App Term Term
  | Let Term Term
  | Pi Name Type Type
  | Type
  | Data DataId [Term]
  | Con ConId [Term]
  | Match Term [Branch]
  | Record [(FieldId, Term)]
  | Project Term FieldId
  | Process ProcessNode
  | Quote NodeId

Le fragment pur doit satisfaire :

bien typé => évaluable
bien typé => non bloqué
bien typé => terminant
forme normale unique

Égalité définitionnelle :

t ≡ u  iff  nf(t) == nf(u)

La récursion générale est interdite.

Récursion autorisée :

- récursion structurelle
- récursion bien fondée
- récursion avec tailles statiques
- coinduction productive
- automates explicitement productifs
7. Évaluation

Protoss utilise une stratégie logique lazy call-by-need, avec annotations de strictness pour optimisation.

Pourquoi lazy :

- compatible avec graph reduction
- compatible avec hash-consing
- évite la normalisation inutile
- transforme les forces en points naturels de cache
- permet le partage maximal des sous-termes

Exemple :

expensive : Int -> Int
expensive x =
    ...

main : Int
main =
    let
        value =
            expensive 42
    in
    1

Ici, expensive 42 n’est jamais forcé.

Annotation possible :

strict sum : List Int -> Int
sum xs =
    ...

Cela indique au compilateur que certains arguments ou accumulateurs peuvent être évalués strictement.

8. Mémoïsation globale

Chaque évaluation pure peut être mémoïsée par contenu.

EvalKey = H(
  "protoss.eval.v1",
  DefId,
  ArgsHash,
  RuntimePolicy
)

Pour les processus dépendant du monde :

EvalKey = H(
  "protoss.eval.v1",
  DefId,
  ArgsHash,
  WorldRef,
  CapScope,
  RuntimePolicy
)

Où :

DefId        identité canonique de la fonction
ArgsHash     hash des arguments canoniques
WorldRef     version du monde observé
CapScope     permissions et secrets disponibles
RuntimePolicy stratégie d’exécution autorisée

Une même fonction appelée avec les mêmes arguments dans le même monde donne la même valeur.

9. Effets

Protoss ne possède pas d’IO implicite.

Les effets sont modélisés comme des processus typés.

type Process caps a
    = Done a
    | Request (Req caps r) (r -> Process caps a)

Exemple :

askName : Process { human.ask } Text
askName =
    Human.ask "Quel est ton nom ?"

Ce programme ne lit pas magiquement une entrée. Il émet une requête canonique :

Request {
  kind = HumanAsk
  prompt = "Quel est ton nom ?"
}

Le runtime inscrit cette requête dans le ledger du monde. Quand une réponse arrive, elle devient un événement signé, hashé, scoped, puis le processus reprend.

10. Monde et ledger

Le monde extérieur est représenté par un Merkle-DAG événementiel.

WorldRef = H(events, derivedStates, branches, merges)

Protoss ne lit pas directement :

- l’heure
- le réseau
- le filesystem
- l’utilisateur
- le hasard
- les capteurs

Il consomme des événements explicitement présents dans un monde versionné.

Exemple :

readSensor : SensorId -> Process { sensor.read } Float
readSensor sid =
    Sensor.read sid

Sémantique :

1. Le programme demande ReadSensor(sid)
2. Le runtime ajoute ou retrouve un événement SensorValue
3. La réponse est injectée dans la continuation
4. Le résultat est déterministe relativement à WorldRef

Si le monde change, WorldRef change, donc le cache ne ment pas.

11. Capabilities

Tous les effets sont bornés par des capacités.

fetchUser : UserId -> Process { net.get "api.users" } User

Une fonction qui n’a pas la capacité réseau ne peut pas faire de requête réseau.

Les capacités sont :

- typées
- explicites
- héritables de manière contrôlée
- visibles dans les signatures
- incluses dans les clés de cache
- vérifiées à l’application des patches MCP

Les secrets ne sont jamais hashés globalement en clair.

Ils sont représentés par des références scellées :

type SecretRef scope a

Le hash global contient l’identité du handle, pas la valeur secrète.

12. Modules

Syntaxe humaine :

module Todo exposing (Todo, toggle)

import List
import Text

type alias Todo =
    { id : Id
    , title : Text
    , done : Bool
    }

toggle : Id -> List Todo -> List Todo
toggle target todos =
    todos
        |> List.map
            (\todo ->
                if todo.id == target then
                    { todo | done = not todo.done }
                else
                    todo
            )

Au niveau canonique :

Module {
  name = "Todo"
  exports = {
    "Todo"   -> TypeId(...)
    "toggle" -> DefId(...)
  }
  imports = {
    "List" -> PackageRef(...)
    "Text" -> PackageRef(...)
  }
}

Les imports peuvent être nominaux ou hashés.

import protoss/core exposing (List, Maybe)
import h:91af23... as StableMath

Le nom StableMath est local. L’identité réelle est h:91af23....

13. Packages

Un package Protoss est une racine Merkle.

Package {
  name        : PackageName
  root        : UniverseRoot
  exports     : ExportMap
  deps        : Map PackageName PackageRef
  policies    : PolicySet
  harnesses   : HarnessSet
}

Versioning :

package@hash       version exacte
package@semver     alias humain vers un hash
package@policy     résolution par contrainte

Exemple :

import protoss/html@2 exposing (Html, div, text)

Résolution réelle :

protoss/html@2 -> h:ab781e...
14. MCP-first

Protoss est conçu pour être modifié par MCP.

L’éditeur, l’IA, le CI, le système de refactor et les migrations parlent tous au graphe via des opérations structurées.

Exemples d’opérations MCP natives :

protoss.query
protoss.readNode
protoss.renderView
protoss.proposePatch
protoss.checkPatch
protoss.applyPatch
protoss.runHarness
protoss.explain
protoss.normalize
protoss.diff
protoss.rollback

Un patch n’est pas une modification textuelle.

C’est une donnée typée :

type Patch
    = AddDef DefDraft
    | ReplaceBody DefId TermDraft
    | Rename MetadataPath Name
    | AddField TypeId FieldSpec Migration
    | RemoveField TypeId FieldId Migration
    | Inline DefId
    | Extract TermPath Name
    | AddHarness DefId Harness
    | AddCapability DefId Capability
    | MigrateType TypeId MigrationPlan

Cycle de vie d’un patch :

1. Intent humain ou IA
2. Proposition MCP
3. Construction d’un patch typé
4. Typecheck
5. Vérification des capacités
6. Vérification de terminaison/productivité
7. Normalisation
8. Hash-consing
9. Exécution du harness
10. Commit d’un nouveau UniverseRoot

Un patch invalide n’entre jamais dans le graphe canonique.

Donc Protoss ne promet pas “aucune erreur pendant l’écriture”. Il promet plutôt :

aucun état invalide admis comme programme Protoss
15. Édition manuelle

Modifier le code à la main n’est pas le workflow principal.

Les fichiers .pt sont :

- des vues
- des exports
- des supports pédagogiques
- des supports de review
- des points de bootstrap

Quand un humain modifie un .pt, le système ne “sauvegarde pas un fichier source”. Il traduit le diff en patch MCP.

text diff -> parsed intent -> patch candidate -> validation -> new graph root

Si la modification textuelle est ambiguë, le patch est refusé avec une erreur d’intention, pas avec une erreur de compilation tardive.

16. Harness IA intégré

Chaque définition peut avoir un harness attaché.

Un harness contient :

- exemples
- tests unitaires
- tests de propriétés
- générateurs de données
- benchmarks
- invariants métier
- contrats de migration
- scénarios de monde
- politiques de sécurité
- prompts de diagnostic
- évaluations IA

Syntaxe humaine :

harness toggle =
    examples
        [ toggle #todo1
            [ { id = #todo1, title = "Spec", done = False } ]
          ==
          [ { id = #todo1, title = "Spec", done = True } ]
        ]

    properties
        [ forall todo.
            toggle todo.id [ todo ]
                |> List.head
                |> Maybe.map .done
                ==
                Just (not todo.done)
        ]

Le harness est lui-même hashé.

HarnessId = H(canonicalBytes(harness))

Lorsqu’une IA propose un patch, elle doit fournir ou mettre à jour le harness correspondant.

17. Rôle natif de l’IA

Dans Protoss, l’IA est un acteur de développement de premier ordre.

Elle peut :

- explorer le graphe
- proposer des patches
- générer des migrations
- synthétiser des tests
- expliquer une définition
- détecter des doublons sémantiques
- factoriser des fonctions identiques
- simuler des changements dans un WorldRef forké
- comparer deux branches par harness
- générer des vues humaines lisibles

Mais elle ne peut pas directement écrire dans le programme canonique.

Elle doit passer par :

IA -> PatchCandidate -> Validator -> Harness -> Commit

Un agent IA n’a donc pas un accès “éditeur texte”. Il a un accès transactionnel au graphe.

18. Exemple de patch MCP

Exemple : ajouter un champ priority à Todo.

{
  "method": "protoss.patch.propose",
  "params": {
    "root": "h:oldRoot",
    "intent": "Add a priority field to Todo with default Normal",
    "operations": [
      {
        "op": "AddField",
        "type": "Todo",
        "field": "priority",
        "fieldType": "Priority",
        "default": "Normal"
      },
      {
        "op": "AddUnionType",
        "name": "Priority",
        "constructors": ["Low", "Normal", "High"]
      },
      {
        "op": "UpdateHarness",
        "target": "Todo",
        "strategy": "preserve existing behavior under default priority"
      }
    ]
  }
}

Le validateur vérifie :

- le type Priority existe
- le champ n’entre pas en conflit
- les constructeurs sont exhaustifs
- les migrations sont totales
- les vues sont régénérables
- les harnesses passent
- les capacités ne changent pas implicitement

Résultat :

oldRoot -> patch -> newRoot
19. Self-hosting

Protoss doit être auto-hébergé.

Cela signifie que les composants suivants sont écrits en Protoss :

- parser de la syntaxe humaine
- pretty-printer
- canonicalizer
- normalizer
- typechecker
- patch validator
- harness runner
- package resolver
- MCP server
- optimizer
- compiler backend

Bootstrap proposé :

Phase 0 : noyau minimal implémenté dans un langage hôte
Phase 1 : Protoss/Core exécutable
Phase 2 : parser + typechecker écrits en Protoss
Phase 3 : compiler Protoss compile lui-même
Phase 4 : remplacement progressif du trusted host
Phase 5 : noyau vérifié ou au moins auditable par hash

Trusted Computing Base minimal :

- algorithme de hash
- format canonique binaire
- vérificateur de types du noyau
- validateur de patches
- runtime des effets

Tout le reste peut être reconstruit depuis le graphe.

20. Compilation

Protoss peut avoir plusieurs backends :

- interprétation graph-reduction
- bytecode Protoss VM
- WebAssembly
- LLVM/native
- JavaScript pour UI
- SQL/dataflow pour requêtes
- GPU kernels pour calcul spécialisé

Mais les artefacts compilés ne sont jamais source de vérité.

CompiledArtifact = derive(UniverseRoot, Target, OptimizationPolicy)

Si deux machines compilent le même UniverseRoot avec la même politique, elles doivent obtenir le même artefact ou un artefact prouvablement équivalent.

21. UI et architecture applicative

La syntaxe humaine peut suivre une architecture proche d’Elm :

type alias Model =
    { todos : List Todo
    , input : Text
    }

type Msg
    = InputChanged Text
    | AddTodo
    | Toggle Id

update : Msg -> Model -> Model
update msg model =
    case msg of
        InputChanged value ->
            { model | input = value }

        AddTodo ->
            { model
                | todos =
                    model.todos
                        |> List.append
                            [ { id = Id.newDeterministic model.input
                              , title = model.input
                              , done = False
                              }
                            ]
                , input = ""
            }

        Toggle id ->
            { model | todos = toggle id model.todos }

Pour les effets :

update : Msg -> Model -> Process { storage.write } Model

Ou séparation stricte :

update : Msg -> Model -> ( Model, Cmd caps Msg )

Mais Cmd est une description canonique d’effet, pas une impureté cachée.

22. Erreurs

Protoss distingue :

- erreurs de patch
- erreurs de validation
- erreurs de harness
- réponses externes négatives
- valeurs métier de type Result

Il n’y a pas d’exception implicite.

Exemples d’erreurs de patch :

TypeMismatch
UnknownReference
CapabilityDenied
NonTerminatingRecursion
NonProductiveProcess
HarnessRegression
AmbiguousHumanSyntax
UnsafeMigration
PolicyViolation
SecretLeakRisk

Une erreur de patch empêche l’insertion dans le graphe.

Une erreur métier est une valeur normale :

parseInt : Text -> Result ParseError Int

Une erreur externe est un événement du ledger :

fetch : Url -> Process { net.get } (Result NetError Response)
23. Sécurité

Protoss applique la sécurité au niveau du type system et du runtime.

Principes :

- capabilities explicites
- secrets scellés
- cache partitionné par CapScope
- imports par hash
- policies attachées aux packages
- patches audités
- provenance native
- aucun accès implicite au monde

Exemple :

sendEmail :
    Email ->
    Process { net.smtp, secret.emailToken } SendResult

Sans secret.emailToken, la fonction ne peut pas être appelée dans ce contexte.

24. Diff et review

Le diff principal est structurel.

Au lieu de :

- count + 1
+ count + 2

Protoss produit :

PatchDiff {
  target = DefId(update)
  change =
    ReplaceLiteral {
      path = body.case.Increment.recordUpdate.count
      from = IntLiteral(1)
      to = IntLiteral(2)
    }
  affected = [
    HarnessId(counterBehavior),
    DefId(viewCounter),
    DefId(counterApp)
  ]
}

La vue humaine peut afficher un diff textuel, mais le diff canonique reste structurel.

25. Relation avec Git

Protoss peut exporter vers Git, mais Git n’est pas le modèle natif.

Équivalence approximative :

Git commit     -> UniverseRoot
Git diff       -> PatchDiff
Git branch     -> World/Universe branch
Git merge      -> canonical merge
Git blame      -> provenance ledger
Git tag        -> named hash

Un repo Protoss peut être représenté comme :

/protoss.lock
/views/**/*.pt
/cache/**/*.ptb
/harness/**/*.pth

Mais ces fichiers sont des projections.

26. Roadmap technique
v0.1 — Core pur
- syntaxe humaine minimale
- syntaxe canonique textuelle
- hash-consing local
- typechecker
- normalizer
- fonctions pures
- ADT
- records
- pattern matching
- harness examples
v0.2 — Store global
- NodeId / DefId stables
- package roots
- imports par hash
- cache d’évaluation
- diff structurel
v0.3 — MCP-first
- serveur MCP Protoss
- query graph
- proposePatch
- checkPatch
- applyPatch
- renderView
- explain
v0.4 — Effects / World ledger
- Process
- capabilities
- WorldRef
- événements
- replay
- fork/merge de mondes
v0.5 — Harness IA
- tests de propriétés
- génération de tests par IA
- validation de patches
- comparaison de candidats
- benchmarks adressés par contenu
v1.0 — Self-hosted
- parser écrit en Protoss
- typechecker écrit en Protoss
- canonicalizer écrit en Protoss
- patch validator écrit en Protoss
- compiler self-hosted
27. Résumé compact

Protoss est un langage où le code est un graphe canonique, pas un texte ; où plusieurs syntaxes peuvent représenter le même programme ; où chaque définition est identifiée par son contenu ; où le hash-consing global permet partage, cache et déduplication universels ; où les effets passent par un ledger versionné ; où les programmes sont modifiés par patches MCP validés plutôt que par édition manuelle ; où l’IA est intégrée comme agent de transformation, test et migration ; et où le système entier doit finir par être écrit en Protoss lui-même.
