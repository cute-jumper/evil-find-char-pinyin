;;; evil-find-char-pinyin.el --- Evil's f/F/t/T commands with Pinyin support  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Junpeng Qiu

;; Author: Junpeng Qiu <qjpchmail@gmail.com>
;; Package-Requires: ((evil "1.2.12") (pinyinlib "0.1.0"))
;; Keywords: extensions

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Table of Contents
;; _________________

;; 1 evil-find-char-pinyin
;; .. 1.1 Installation
;; .. 1.2 Usage
;; .. 1.3 Config
;; ..... 1.3.1 Enable Traditional Chinese Support
;; ..... 1.3.2 Disable Punctuaction Support


;; 1 evil-find-char-pinyin
;; =======================

;;   Evil's f/F/t/T commands with Pinyin support.


;; 1.1 Installation
;; ~~~~~~~~~~~~~~~~

;;   ,----
;;   | (add-to-list 'load-path "/path/to/evil-find-char-pinyin.el")
;;   | (require 'evil-find-char-pinyin)
;;   `----


;; 1.2 Usage
;; ~~~~~~~~~

;;   To enable the mode:
;;   ,----
;;   | (evil-find-char-pinyin-mode +1)
;;   `----

;;   After you enable the mode, `evil''s f/F/t/T commands are able to jump
;;   to Chinese characters by their Pinyin. You can also use `;' and `,' to
;;   repeat the last f/F/t/T command.

;;   For example (`|' is the location of the cursor):
;;   ,----
;;   | |我能吞下玻璃而不伤身体。
;;   `----

;;   `dft' will delete `我能吞' .

;;   It also supports Chinese punctuactions:
;;   ,----
;;   | |我能吞下玻璃而不伤身体。
;;   `----

;;   `dt.' will delete up to `。'.

;;   Here is the full list of punctuation mappings:
;;    English Punctuation  Chinese & English Punctuations
;;   -----------------------------------------------------
;;    .                    。.
;;    ,                    ，,
;;    ?                    ？?
;;    :                    ：:
;;    !                    ！!
;;    ;                    ；;
;;    \\                   、\\
;;    (                    （(
;;    )                    ）)
;;    <                    《<
;;    >                    》>
;;    ~                    ～~
;;    '                    ‘’「」'
;;    "                    “”『』\"
;;    *                    ×*
;;    $                    ￥$

;;   The English punctuaction keys in the first column can be used to find
;;   the Chinese & English punctuactions in the second column.


;; 1.3 Config
;; ~~~~~~~~~~

;; 1.3.1 Enable Traditional Chinese Support
;; ----------------------------------------

;;   By default, only Simplifed Chinese charaters are supported. To enable
;;   Traditional Chinese support:
;;   ,----
;;   | (setq evil-find-char-pinyin-only-simplified nil)
;;   `----


;; 1.3.2 Disable Punctuaction Support
;; ----------------------------------

;;   If you don't want the punctuation support, use:
;;   ,----
;;   | (setq evil-find-char-pinyin-enable-punctuation-translation nil)
;;   `----
;;

;;; Code:

(require 'evil)
(require 'pinyinlib)

(defvar evil-find-char-pinyin-only-simplified t
  "Whether we only deal with simplified Chinese character or not.")

(defvar evil-find-char-pinyin-enable-punctuation-translation t
  "Enable punctuation support or not.")

(defun evil-find-char-pinyin--build-regexp (char)
  (pinyinlib-build-regexp-char
   char
   (not evil-find-char-pinyin-enable-punctuation-translation)
   (not evil-find-char-pinyin-only-simplified)))

(evil-define-motion evil-find-char-pinyin (count char)
  "Move to the next COUNT'th occurrence of CHAR."
  :type inclusive
  (interactive "<c><C>")
  (setq count (or count 1))
  (let ((fwd (> count 0)))
    (setq evil-last-find (list #'evil-find-char-pinyin char fwd))
    (when fwd (forward-char))
    (let ((case-fold-search nil))
      (unless
          (prog1
              (search-forward-regexp
               (evil-find-char-pinyin--build-regexp char)
               (unless evil-cross-lines
                 (if fwd
                     (line-end-position)
                   (line-beginning-position)))
               t count)
            (when fwd (backward-char)))
        (user-error "Can't find %c" char)))))

(evil-define-motion evil-find-char-pinyin-backward (count char)
  "Move to the previous COUNT'th occurrence of CHAR."
  :type exclusive
  (interactive "<c><C>")
  (evil-find-char-pinyin (- (or count 1)) char))

(evil-define-motion evil-find-char-pinyin-to (count char)
  "Move before the next COUNT'th occurrence of CHAR."
  :type inclusive
  (interactive "<c><C>")
  (unwind-protect
      (progn
        (evil-find-char-pinyin count char)
        (if (> (or count 1) 0)
            (backward-char)
          (forward-char)))
    (setcar evil-last-find #'evil-find-char-pinyin-to)))

(evil-define-motion evil-find-char-pinyin-to-backward (count char)
  "Move before the previous COUNT'th occurrence of CHAR."
  :type exclusive
  (interactive "<c><C>")
  (evil-find-char-pinyin-to (- (or count 1)) char))

(evil-define-motion evil-repeat-find-char-pinyin (count)
  "Repeat the last find COUNT times."
  :type inclusive
  (setq count (or count 1))
  (if evil-last-find
      (let ((cmd (car evil-last-find))
            (char (nth 1 evil-last-find))
            (fwd (nth 2 evil-last-find))
            evil-last-find)
        ;; ensure count is non-negative
        (when (< count 0)
          (setq count (- count)
                fwd (not fwd)))
        ;; skip next character when repeating t or T
        (and (eq cmd #'evil-find-char-pinyin-to)
             evil-repeat-find-to-skip-next
             (= count 1)
             (or (and fwd (or (= (char-after (1+ (point))) char)
                              (string-match-p
                               (evil-find-char-pinyin--build-regexp char)
                               (string (char-after (1+ (point)))))))
                 (and (not fwd) (or (= (char-before) char)
                                    (string-match-p
                                     (evil-find-char-pinyin--build-regexp char)
                                     (string (char-before))))))
             (setq count (1+ count)))
        (funcall cmd (if fwd count (- count)) char)
        (unless (nth 2 evil-last-find)
          (setq evil-this-type 'exclusive)))
    (user-error "No previous search")))

(evil-define-motion evil-repeat-find-char-pinyin-reverse (count)
  "Repeat the last find COUNT times in the opposite direction."
  :type inclusive
  (evil-repeat-find-char-pinyin (- (or count 1))))

;;;###autoload
(define-minor-mode evil-find-char-pinyin-mode
  "Minor mode to make Evil's f/F/t/T be able to find Chinese characters."
  :global t
  :lighter " EFCP"
  (if evil-find-char-pinyin-mode
      (progn
        (define-key evil-motion-state-map
          [remap evil-find-char] 'evil-find-char-pinyin)
        (define-key evil-motion-state-map
          [remap evil-find-char-backward] 'evil-find-char-pinyin-backward)
        (define-key evil-motion-state-map
          [remap evil-find-char-to] 'evil-find-char-pinyin-to)
        (define-key evil-motion-state-map
          [remap evil-find-char-to-backward] 'evil-find-char-pinyin-to-backward)
        (define-key evil-motion-state-map
          [remap evil-find-char-to-backward] 'evil-find-char-pinyin-to-backward)
        (define-key evil-motion-state-map
          [remap evil-repeat-find-char] 'evil-repeat-find-char-pinyin)
        (define-key evil-motion-state-map
          [remap evil-repeat-find-char-reverse] 'evil-repeat-find-char-pinyin-reverse))
    (define-key evil-motion-state-map [remap evil-find-char] nil)
    (define-key evil-motion-state-map [remap evil-find-char-backward] nil)
    (define-key evil-motion-state-map [remap evil-find-char-to] nil)
    (define-key evil-motion-state-map [remap evil-find-char-to-backward] nil)
    (define-key evil-motion-state-map [remap evil-repeat-find-char] nil)
    (define-key evil-motion-state-map [remap evil-repeat-find-char-reverse] nil)))

(provide 'evil-find-char-pinyin)
;;; evil-find-char-pinyin.el ends here
