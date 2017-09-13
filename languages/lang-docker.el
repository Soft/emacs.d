;; -*- lexical-binding: t -*-
;; Docker

(use-package docker
  :ensure t
  :defer t)

(use-package dockerfile-mode
  :mode (("Dockerfile" . dockerfile-mode))
  :ensure t)

(use-package docker-compose-mode
  :mode (("docker-compose.*\.yml\\'" . docker-compose-mode))
  :ensure t)

(provide 'lang-docker)
