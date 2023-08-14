;;; package --- Sumarry Configuração Emacss: Wendellast

;;; GLOBAL =-=-=-=-=-==-=-=-=-=-==-=-=-=-=-==-=-=-=-=-==-=-=-=-=-==-=-=-=-=-==-=-=-=-=-==-=-=-=-=-==-=-=-=-=-=

(setq inhibit-startup-message t)     ;;Adeus, buffer assustador!
(tool-bar-mode   -1)                 ; Oculta a barra de ferramentas
(menu-bar-mode   1)                  ; Oculta a barra de menu
(scroll-bar-mode -1)                 ; Oculta a barra de rolagem
(tooltip-mode    -1)                 ; Oculta dicas

(global-display-line-numbers-mode t) ; Exibe numeração de linhas
(column-number-mode t)               ; Exibe coluna atual na modeline
(global-hl-line-mode t)              ; Exibe destaque de linha
(setq visible-bell t)                ; Alertas visuais

(set-fringe-mode 10)                ; Espaçamento das bordas laterais
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "<Scroll_Lock>")) ; Inibe Ctrl-Z (suspend frame)
(delete-selection-mode t)                  ; O texto digitado substitui a seleção

;; Rolagem mais suave
(setq mouse-wheel-scroll-amount '(2 ((shift) . 1))  ; 2 linhas por vez
      mouse-wheel-progressive-speed nil             ; Não acelera a rolagem
      mouse-wheel-follow-mouse 't                   ; Rola a janela sob o mouse
      scroll-step 1)                                ; Rola 1 linha com teclado

;; Quebras de linha
(global-visual-line-mode t)

;; Theme
(load-theme 'tango-dark t)

;; Tipo de cursor (box, bar ou hbar)
(setq-default cursor-type 'box)

;; Fonte padrão
;; (set-face-attribute 'default nil :font "Roboto Mono" :height 120) ;

;; Função para criar um novo buffer
(defun debmx-new-buffer ()
  "Cria um novo buffer `sem nome'."
  (interactive)
  (let ((debmx/buf (generate-new-buffer "sem-nome")))
    (switch-to-buffer debmx/buf)
    (funcall initial-major-mode)
    (setq buffer-offer-save t)
    debmx/buf))

;; Modo inicial
(setq initial-major-mode 'prog-mode)
(setq initial-buffer-choice 'debmx-new-buffer)

;; Organizando os backups
(setq backup-directory-alist `(("." . "~/.saves")))

;; Atalhos
(global-set-key (kbd "C-<tab>") 'other-window)
(global-set-key (kbd "M-<down>") 'enlarge-window)
(global-set-key (kbd "M-<up>") 'shrink-window)
(global-set-key (kbd "M-<left>") 'enlarge-window-horizontally)
(global-set-key (kbd "M-<right>") 'shrink-window-horizontally)

;; PACKAGE =-=-=-=-=-==-=-=-=-=-==-=-=-=-=-==-=-=-=-=-==-=-=-=-=-==-=-=-=-=-==-=-=-=-=-==-=-=-=-=-==-=-=-=-=-=

;; Verifica e inicia o package.el
(require 'package)

;; Definição de repositórios
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org"   . "https://orgmode.org/elpa/")
                         ("elpa"  . "https://elpa.gnu.org/packages/")))

;; Inicialização do sistema de pacotes
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Instalação do use-package
(unless (package-installed-p 'use-package)
  (require 'use-package)
  (setq use-package-always-ensure t))

(use-package try
  :ensure t)

(use-package auto-complete
  :ensure t
  :init
  (progn
    (ac-config-default)
    (global-auto-complete-mode t)))

(use-package all-the-icons
  :ensure t
  :if (display-graphic-p))

(use-package neotree
  :ensure t
  :config
  (progn
    (setq neo-theme (if (display-graphic-p) 'arrow)))
  :bind (("C-q" . 'neotree-toggle))) ; Atalho corrigido para "C-x q"

(use-package ace-window
  :ensure t
  :bind (("C-x o" . ace-window)))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode t))

(use-package rebecca-theme
  :ensure t
  :config
  (load-theme 'rebecca t))

(use-package auto-package-update
  :custom
  (auto-package-update-interval 7)
  (auto-package-update-prompt-before-update t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe)
  (auto-package-update-at-time "21:00"))

;; Salva o diretório atual ao fechar o Emacs
(add-hook 'kill-emacs-hook
          (lambda ()
            (setq meu-ultimo-diretorio default-directory)
            (with-temp-file "~/.emacs_last_directory"
              (insert meu-ultimo-diretorio))))

;; Restaura o último diretório ao iniciar o Emacs
(when (file-readable-p "~/.emacs_last_directory")
  (setq meu-ultimo-diretorio (with-temp-buffer
                               (insert-file-contents "~/.emacs_last_directory")
                               (buffer-string)))
  (delete-file "~/.emacs_last_directory")
  (setq default-directory meu-ultimo-diretorio))

(custom-set-variables
 '(ispell-dictionary "brasileiro")
 '(package-selected-packages
   '(ace-window all-the-icons auto-complete auto-package-update ergoemacs-mode ergoesmacs-mode flycheck neotree rebecca-theme try use-package)))
(custom-set-faces)
