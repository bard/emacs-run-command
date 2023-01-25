;;; run-command-recipe-cargo.el --- Recipe for cargo (Rust project manager) commands -*- lexical-binding: t -*-

;;; Commentary:

;; Recipe for cargo (Rust project manager) commands.

;;; Code:

(require 'subr-x)

(defun run-command-recipe-cargo ()
  "Provide commands for a Rust project managed with `cargo'."
  (when-let* ((project-dir
               (locate-dominating-file default-directory "Cargo.toml")))
    `((:command-name
       "test:watch"
       :command-line "cargo watch --watch src --watch Cargo.toml --clear -x test"
       :display "test:watch"
       :working-dir ,project-dir)
      (:command-name
       "build"
       :command-line "cargo build"
       :display "build"
       :working-dir ,project-dir)
      (:command-name
       "build:watch"
       :command-line "cargo watch --watch src --watch Cargo.toml --clear -x build"
       :display "build:watch"
       :working-dir ,project-dir)
      (:command-name
       "run"
       :command-line "cargo run"
       :display "run"
       :working-dir ,project-dir)
      (:command-name
       "run:watch"
       :command-line "cargo watch --watch src --watch Cargo.toml --clear -x run"
       :display "run:watch"
       :working-dir ,project-dir)
      (:command-name
       "lint"
       :command-line "cargo clippy"
       :display "lint"
       :working-dir ,project-dir))))

(provide 'run-command-recipe-cargo)
;;; run-command-recipe-cargo.el ends here
