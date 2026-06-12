import Verso
import VersoManual.InlineLean.Signature

open Verso ArgParse Doc
open Lean Elab

section Slink

variable [Monad m] [MonadInfoTree m] [MonadLiftT CoreM m] [MonadEnv m] [MonadError m]

/-- An edit link is a link which opens an editor, specifically vscode,
    to the specified path. -/
structure EditLink where
  path : String

instance : FromArgs EditLink m where
  fromArgs := EditLink.mk  <$> positional' `path

/-- Define the `editlink` role. It expands to a link which opens an editor, specifically vscode,
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

/-- A src link is a link which opens an and editor, specifically vscode,
    to the specified path, using an environment variable to resolve
    relative paths. -/
structure SrcLink where
  path : String

instance : FromArgs SrcLink m where
  fromArgs := SrcLink.mk  <$> positional' `path

/-- Define the `srclink` role. It expands to a link which opens an editor, specifically vscode,
    to the specified path. If the path is relative, it is resolved with respect to the SOURCE_ROOT
    at the time the verso project is built. If the absolute path does not exist or is not a file, an error is thrown.
-/
@[role]
def srclink : RoleExpanderOf SrcLink
  | { path }, args => do
    let fsPath := System.FilePath.mk path
    let root? ← IO.getEnv "SOURCE_ROOT"
    let root ← match root? with
      | some r => pure r
      | none => throwError "SOURCE_ROOT environment variable is not set, cannot resolve relative paths for srclink role."
    let absPath := if fsPath.isRelative then System.FilePath.mk (s!"{root}" ++ "/" ++ path) else fsPath
    let isFile := (← absPath.pathExists) && ! (← absPath.isDir)
    if ! isFile then
      throwError s!"{path} interpreted as {absPath} either does not exist or is not a file."
    ``(Inline.link #[$[$(← args.mapM elabInline)],*] $(quote <| "vscode:" ++ s!"{absPath}" ))


end Slink
