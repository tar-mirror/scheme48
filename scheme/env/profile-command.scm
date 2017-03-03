; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Marcel Turino, Manuel Dietrich

(define-user-command-syntax 'profile "<command>" "profile execution"
  '(command))

(environment-define! (user-command-environment)
		     'profile
		     profile)
