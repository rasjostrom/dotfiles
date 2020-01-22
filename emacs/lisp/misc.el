(provide 'misc)

(defun misc/cut-path ()
  "Push full path of the current file to kill ring."
  (interactive)
  (kill-new (buffer-file-name)))


(defun misc/revert-buffer-no-prompt ()
  "Reverts buffer without prompting."
  (interactive)
  (revert-buffer t t))


(defun install-packages-recursive (lop)
  "Recursively installs any package in the list 'lop' if it isn't already."
  (cond
   ((null lop) t)
   ((package-installed-p (car lop)) (install-packages-by-list (cdr lop)))
   ((progn
     (package-install (car lop))
     (install-packages-by-list (cdr lop))))))


(defun reinstall-packages (lop)
  (cond
   ((null lop) t)
   ((progn
      (package-reinstall (car lop))
      (reinstall-packages (cdr lop))))))


(defun org-show-current-heading-tidily ()
  "Show next entry, keeping other entries closed."
  (interactive)
  (if (save-excursion (end-of-line) (outline-invisible-p))
      (progn (org-show-entry) (show-children))
    (outline-back-to-heading)
    (unless (and (bolp) (org-on-heading-p))
      (org-up-heading-safe)
      (outline-hide-subtree)
      (error "Boundary reached"))
    (org-overview)
    (org-reveal t)
    (org-show-entry)
    (show-children)))

(defun lorem-ipsum ()
  (interactive)
  (let ((length (read-number "Length (<2550): "))
	(lorem "Lorem ipsum dolor sit amet, consectetur
adipiscing elit. Curabitur ultricies, erat at facilisis
vulputate, est nisl pellentesque nibh, sed rhoncus orci nunc in
dolor. Quisque pharetra interdum quam, eget sagittis ligula
faucibus ac. Etiam molestie, nunc ac pellentesque pretium, justo
metus mattis mi, eget euismod ante sem sed leo. Maecenas lacinia
facilisis mi, vel ornare risus pharetra ac. Maecenas quis urna
congue, vestibulum dolor sed, mollis eros. Nam volutpat, purus ut
condimentum eleifend, nunc nisl sodales justo, ac efficitur est
eros in libero. Sed lectus libero, pharetra vel ex et, porta
fermentum eros. Praesent ac iaculis augue, et semper
sapien. Etiam maximus porttitor nisl. Phasellus feugiat tortor et
arcu sollicitudin, non mattis enim auctor.

Nam consectetur gravida luctus. Maecenas quis est at velit
maximus facilisis vitae nec tortor. Integer ac tortor eu libero
tincidunt mattis. In a commodo massa. Aenean et magna molestie,
sollicitudin nulla nec, porta felis. Nunc maximus ligula in
egestas fringilla. Sed ut volutpat arcu, non dictum est.

Curabitur gravida magna velit, quis consequat enim posuere
eget. Etiam quis nisi ante. Donec hendrerit, dolor vitae pulvinar
pulvinar, sapien ligula tempus libero, sed scelerisque enim odio
vitae mi. Fusce imperdiet congue mauris quis ultrices. Etiam
ornare facilisis dolor vel volutpat. Nulla non auctor velit, sed
hendrerit nisi. Fusce dictum vel metus tempor lacinia.

Phasellus est eros, dignissim non est non, auctor commodo
enim. Praesent est ipsum, mollis non neque sed, ultrices
facilisis ex. Cras dignissim, sem quis auctor rhoncus, enim orci
euismod metus, vitae varius mi sem ullamcorper justo. Aenean
lobortis, risus non congue accumsan, neque libero ultricies enim,
quis aliquam lacus turpis sit amet eros. Phasellus sit amet orci
et mauris elementum fermentum quis ut lacus. Aenean auctor leo ac
ligula pretium, at facilisis diam lacinia. Nam vehicula malesuada
dapibus.

Curabitur in tempus justo, a finibus metus. Curabitur consectetur
nunc a laoreet semper. Sed at quam accumsan magna gravida
elementum vitae ut mi. In ac nisi nibh. Nam id nunc eu augue
rutrum sagittis. Vivamus lorem urna, feugiat ut luctus quis,
porttitor ut arcu. Sed eget porttitor tellus. Duis quis elementum
purus. Proin interdum ultricies est, a sodales ex sodales
quis. Nulla scelerisque massa mi, sed euismod nibh lobortis
eget. Suspendisse dignissim non elit nec tempus. Mauris
consectetur sodales tellus in suscipit. Sed vel hendrerit
dui. Nam sollicitudin mi a sollicitudin blandit."))
    (insert
     (substring
      lorem 0 (if (< length (length lorem)) length (length lorem))))))
