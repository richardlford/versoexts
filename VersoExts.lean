import Verso
import VersoManual.InlineLean.Signature

open Verso ArgParse Doc
open Lean Elab

section Slink

variable [Monad m] [MonadInfoTree m] [MonadLiftT CoreM m] [MonadEnv m] [MonadError m]

/-- And edit link is a link which opens an editor, specifically vscode,
    to the specified path. -/
structure EditLink where
  path : String

instance : FromArgs EditLink m where
  fromArgs := EditLink.mk  <$> positional' `path

/-- Define an `editlink` role. It expands to a link which opens an editor, specifically vscode,
    to the specified path. If the path is relative, it is resolved with respect to the current working directory at the time the verso project is built. If the absolute path does not exist or is not a file, an error is thrown.
-/
@[role]
def editlink : RoleExpanderOf EditLink
  | { path }, args => do
    let fsPath := System.FilePath.mk path
    let cwd ← IO.currentDir
    let absPath := if fsPath.isRelative then System.FilePath.mk (s!"{cwd}" ++ "/" ++ path) else fsPath
    let isFile := (← absPath.pathExists) && ! (← absPath.isDir)
    if ! isFile then
      throwError s!"{path} interpreted as {absPath} either does not exist or is not a file."
    ``(Inline.link #[$[$(← args.mapM elabInline)],*] $(quote <| "vscode:" ++ s!"{absPath}" ))

end Slink
