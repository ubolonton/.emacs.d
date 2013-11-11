(let* ((name "* font test *")
       (buffer (get-buffer name)))
  (when buffer
    (kill-buffer buffer))
  (with-current-buffer (switch-to-buffer name)
    (font-lock-mode +1)
    (goto-char (point-max))
    (dolist
        (f '(variable-pitch font-lock-string-face))
      (dolist
          (s (list
              "a b c d e f g h i j k l m n o p q r s t u v w x y z"
              "a á à ả ã ạ   â ấ ầ ẩ ẫ ậ   ă ắ ằ ẳ ẵ ậ"
              "o ó ò ỏ õ ọ   ô ố ồ ổ ỗ ộ   ơ ớ ờ ở ỡ ợ"
              "e é è ẻ ẽ ẹ   ê ế ề ể ễ ệ"
              "u ú ù ủ ũ ụ                 ư ứ ừ ử ữ ự"
              "i í ì ỉ ĩ ị   y ý ỳ ỷ ỹ ỵ   đ"
              "а б в г д е ё ж з и й к л м н о п р с т у ф х ц ч ш щ ъ ы ь э ю я"
              "А Б В Г Д Е Ё Ж З И Й К Л М Н О П Р С Т У Ф Х Ц Ч Ш Щ Ъ Ы Ь Э Ю Я"
              "The wizard quickly jinxed the gnomes before they vaporized."
              "Do bạch kim rất quý, sẽ để lắp vô xương."
              "Do bach kim rat quy, se de lap vo xuong."
              "Широкая электрификация южных губерний даст мощный толчок подъёму сельского хозяйства."
              ;; This is not even transliteration, it's for
              ;; comparing length (fixed-width)
              "Sirokay electrifikasia yjnyi gubernyi dast mosnyi toljok podyemu selsskovo kozaistva."
              ))
        (insert "\n| ")
        (insert (propertize s 'font-lock-face f)))
      (insert "\n\n"))
    (dolist (s (list
                "<= <= <= <= <= <= <= <= <= <= <= <= <= <= <= <="
                "⇐ ⇐ ⇐ ⇐ ⇐ ⇐ ⇐ ⇐ ⇐ ⇐ ⇐ ⇐ ⇐ ⇐ ⇐ ⇐"
                "=> => => => => => => => => => => => => => => =>"
                "⇒ ⇒ ⇒ ⇒ ⇒ ⇒ ⇒ ⇒ ⇒ ⇒ ⇒ ⇒ ⇒ ⇒ ⇒ ⇒"
                "return return return return return return"
                "▸▸ ▸▸ ▸▸ ▸▸ ▸▸ ▸▸ ▸▸ ▸▸ ▸▸ ▸▸ ▸▸ ▸▸ ▸▸ ▸▸"
                "function function function function"
                "ƒ ƒ ƒ ƒ ƒ ƒ ƒ ƒ ƒ ƒ ƒ ƒ ƒ ƒ ƒ ƒ ƒ ƒ"
                "lambda lambda lambda lambda lambda lambda"
                "λ λ λ λ λ λ λ λ λ λ λ λ λ λ λ λ λ λ λ λ λ"
                ))
      (insert "\n| ")
      (insert (propertize s 'font-lock-face 'font-lock-keyword-face)))))
