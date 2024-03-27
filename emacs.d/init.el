  ;;; package --- Summary: Configuração Emacs: Wendellast

;;; GLOBAL =-=-=-=-=-==-=-=-=-=-==-=-=-=-=-==-=-=-=-=-==-=-=-=-=-==-=-=-=-=-==-=-=-=-=-==-=-=-=-=-==-=-=-=-=-=

;; Desativa a mensagem de inicialização
(setq inhibit-startup-message t)

;; Oculta barras de ferramentas e de menu
(tool-bar-mode   -1)
(menu-bar-mode   -1)

;; Oculta a barra de rolagem e dicas
(scroll-bar-mode -1)
(tooltip-mode    -1)

;; Ativa a exibição de números de linha, destaque de linha e coluna atual
(global-display-line-numbers-mode t)
(column-number-mode t)
(global-hl-line-mode t)

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))


;; Alertas visuais
(setq visible-bell t)






;; TAB
;; Configuração de espaçamento de tabulação
(setq-default indent-tabs-mode nil) ;; Usa espaços em vez de tabulações
(setq-default tab-width 4) ;; Define o tamanho da tabulação como 4 espaços
(setq-default c-basic-offset 4) ;; Define o tamanho da tabulação para arquivos de código C-like (opcional)
(setq-default js-indent-level 4) ;; Define o tamanho da tabulação para arquivos JavaScript (opcional)
	

;; Espaçamento das bordas laterais
(set-fringe-mode 10)

;; Desabilita Ctrl-Z (suspensão do frame) e modo de seleção
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "<Scroll_Lock>"))
(delete-selection-mode t)

;; Rolagem suave
(setq mouse-wheel-scroll-amount '(2 ((shift) . 1))
      mouse-wheel-progressive-speed nil
      mouse-wheel-follow-mouse 't
      scroll-step 1)

;; Quebras de linha visual
(global-visual-line-mode t)

;; Tipo de cursor (box, bar ou hbar)
(setq-default cursor-type 'box)

;; Função para criar um novo buffer
(defun debmx-new-buffer ()
  "Cria um novo buffer sem nome."
  (interactive)
  (let ((debmx/buf (generate-new-buffer "sem-nome")))
    (switch-to-buffer debmx/buf)
    (funcall initial-major-mode)
    (setq buffer-offer-save t)
    debmx/buf))

;; Ativar o modo de destaque de sintaxe
(global-font-lock-mode t)

;; Modo inicial
(setq initial-major-mode 'prog-mode)
(setq initial-buffer-choice 'debmx-new-buffer)

;; Organizando os backups
(setq backup-directory-alist `(("." . "~/.saves")))

;; Atalhos
(global-set-key (kbd "C-x <tab>") 'other-window)
(global-set-key (kbd "M-<down>") 'enlarge-window)
(global-set-key (kbd "M-<up>") 'shrink-window)
(global-set-key (kbd "M-<left>") 'enlarge-window-horizontally)
(global-set-key (kbd "M-<right>") 'shrink-window-horizontally)

;;; PACKAGE =-=-=-=-=-==-=-=-=-=-==-=-=-=-=-==-=-=-=-=-==-=-=-=-=-==-=-=-=-=-==-=-=-=-=-==-=-=-=-=-==-=-=-=-=-=

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
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; Instalação dos pacotes adicionais

(use-package try :ensure t)

(use-package auto-complete :ensure t
  :init (progn
          (ac-config-default)
          (global-auto-complete-mode t)))
(use-package all-the-icons :ensure t
  :if (display-graphic-p))
(use-package neotree :ensure t
  :config (setq neo-theme (if (display-graphic-p) 'arrow))
  :bind (("C-x q" . 'neotree-toggle))) ; Corrige o atalho para "C-x q"
(use-package ace-window :ensure t :bind (("C-x o" . ace-window)))
(use-package flycheck :ensure t :init (global-flycheck-mode t))


(use-package all-the-icons :ensure t)

(use-package neotree
  :ensure t
  :config
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  (setq neo-window-width 35) ;; Ajuste conforme necessário
  (setq neo-smart-open t) ;; Abre automaticamente o NeoTree no diretório do arquivo atual
  (setq neo-autorefresh nil) ;; Desativa a atualização automática do NeoTree
  :bind ("<f8>" . 'neotree-toggle) ;; Atalho para ativar/desativar o NeoTree
)


(use-package company-jedi
  :ensure t
  :after python
  :config
  (add-to-list 'company-backends 'company-jedi))

(use-package anaconda-mode
  :ensure t
  :hook (python-mode . anaconda-mode)
  :config
  (anaconda-eldoc-mode))


(use-package go-mode
  :ensure t
  :config
  (add-hook 'go-mode-hook 'lsp-deferred)
  (add-hook 'go-mode-hook 'flycheck-mode)
  (add-hook 'go-mode-hook 'company-mode)
  (add-hook 'go-mode-hook 'go-eldoc-setup))


(use-package robe
  :ensure t
  :hook (ruby-mode . robe-mode)
  :config
  (add-to-list 'company-backends 'company-robe))

(use-package inf-ruby
  :ensure t
  :hook (ruby-mode . inf-ruby-minor-mode))



(unless (package-installed-p 'kanagawa-theme)
  (package-install 'kanagawa-theme))

;; Ativar o tema Kanagawa
(load-theme 'kanagawa t)


(use-package auto-package-update :ensure t
  :custom
  (auto-package-update-interval 7)
  (auto-package-update-prompt-before-update t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe)
  (auto-package-update-at-time "21:00"))
  
  
  
 ;; ASPAS
 
 (use-package smartparens
  :ensure t
  :config
  (smartparens-global-mode t))

(defun insert-pair ()
  "Insert a pair of characters and place point between them."
  (interactive)
  (let ((pair (read-string "Enter pair to insert: ")))
    (insert pair)
    (save-excursion
      (insert (cdr (assoc (string pair) '(("(" . ")")
                                           ("[" . "]")
                                           ("{" . "}")
                                           ("<" . ">")
                                           ("\"" . "\""))))))))

(global-set-key (kbd "M-9") #'insert-pair)


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
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ispell-dictionary "brasileiro")
 '(package-selected-packages
   '(ace-window all-the-icons auto-complete auto-package-update ergoemacs-mode ergoesmacs-mode flycheck neotree try use-package)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

