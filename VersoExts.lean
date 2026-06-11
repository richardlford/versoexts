import Verso
import VersoManual.InlineLean.Signature

open Verso ArgParse Doc
open Lean Elab

section Slink

variable [Monad m] [MonadInfoTree m] [MonadLiftT CoreM m] [MonadEnv m] [MonadError m]


structure SrcLink where
  path : String

instance : FromArgs SrcLink m where
  fromArgs := SrcLink.mk  <$> positional' `path

@[role]
def srclink : RoleExpanderOf SrcLink
  | { path }, args => do
    let fsPath := System.FilePath.mk path
    let cwd ← IO.currentDir
    let absPath := if fsPath.isRelative then System.FilePath.mk (s!"{cwd}" ++ "/" ++ path) else fsPath
    let isFile := (← absPath.pathExists) && ! (← absPath.isDir)
    if ! isFile then
      throwError s!"{path} interpreted as {absPath} either does not exist or is not a file."
    ``(Inline.link #[$[$(← args.mapM elabInline)],*] $(quote <| "vscode:" ++ s!"{absPath}" ))

end Slink
