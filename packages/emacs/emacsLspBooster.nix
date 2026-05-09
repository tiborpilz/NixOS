{ pkgs, lib, ... }:

let
  emacs-lsp-booster = pkgs.rustPlatform.buildRustPackage
    rec {
      pname = "emacs-lsp-booster";
      version = "0.2.1";

      cargoHash = "sha256-BR0IELLzm+9coaiLXQn+Rw6VLyiFEAk/nkO08qPwAac=";

      src = pkgs.fetchFromGitHub {
        owner = "blahgeek";
        repo = pname;
        rev = "v${version}";
        hash = "sha256:uP/xJfXQtk8oaG5Zk+dw+C2fVFdjpUZTDASFuj1+eYs=";
      };

      doCheck = false;

      meta = with lib; {
        description = "Improve performance of Emacs LSP servers by converting JSON to bytecode";
        homepage = "https://github.com/${src.owner}/${pname}";
        changelog = "https://github.com/${src.owner}/${pname}/releases/tag/${version}";
        license = [ licenses.mit ];
        maintainers = [ ];
        mainProgram = "emacs-lsp-booster";
      };
    };
in
{
  emacs-lsp-booster = emacs-lsp-booster;
}
#
# (defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
#   "Prepend emacs-lsp-booster command to lsp CMD."
#   (let ((orig-result (funcall old-fn cmd test?)))
#     (if (and (not test?)                             ;; for check lsp-server-present?
#              (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
#              lsp-use-plists
#              (not (functionp 'json-rpc-connection))  ;; native json-rpc
#              (executable-find "emacs-lsp-booster"))
#         (progn
#           (when-let ((command-from-exec-path (executable-find (car orig-result))))  ;; resolve command from exec-path (in case not found in $PATH)
#             (setcar orig-result command-from-exec-path))
#           (message "Using emacs-lsp-booster for %s!" orig-result)
#           (cons "emacs-lsp-booster" orig-result))
#       orig-result)))
# (advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)
#
# (defun lsp-booster--advice-json-parse (old-fn &rest args)
#   "Try to parse bytecode instead of json."
#   (or
#    (when (equal (following-char) ?#)
#      (let ((bytecode (read (current-buffer))))
#        (when (byte-code-function-p bytecode)
#          (funcall bytecode))))
#    (apply old-fn args)))
# (advice-add (if (progn (require 'json)
#                        (fboundp 'json-parse-buffer))
#                 'json-parse-buffer
#               'json-read)
#             :around
#             #'lsp-booster--advice-json-parse)
#
# (defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
#   "Prepend emacs-lsp-booster command to lsp CMD."
#   (let ((orig-result (funcall old-fn cmd test?)))
#     (if (and (not test?)                             ;; for check lsp-server-present?
#              (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
#              lsp-use-plists
#              (not (functionp 'json-rpc-connection))  ;; native json-rpc
#              (executable-find "emacs-lsp-booster"))
#         (progn
#           (when-let ((command-from-exec-path (executable-find (car orig-result))))  ;; resolve command from exec-path (in case not found in $PATH)
#             (setcar orig-result command-from-exec-path))
#           (message "Using emacs-lsp-booster for %s!" orig-result)
#           (cons "emacs-lsp-booster" orig-result))
#       orig-result)))
# (advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)
