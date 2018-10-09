;; helm-snippets.el --- Library for creating Helm prompts for inserting snippets  -*- lexical-binding: t -*-

;; Package-Requires: ((emacs "26.1") (helm))

;;; Commentary:

;; Tiny library for creating Helm prompts for inserting snippets.

;;; Code:

;;;###autoload
(defmacro helm-snippets-define (name doc &rest snippets)
  "Define Helm prompt that allows inserting a string from
`snippets'."
  (declare (indent defun))
  `(defun ,name ()
     ,doc
     (interactive)
     (helm :sources (helm-build-sync-source ,(symbol-name name)
                      :candidates (list ,@snippets)
                      :action
                      (helm-make-actions "Insert" #'insert))
           :buffer ,(format "*%s*" (symbol-name name)))))

;;;###autoload
(helm-snippets-define helm-snippets-arrow
  "Insert Unicode arrows using Helm."
  "←" "→" "↑" "↓" "↔" "↕" "↖" "↗" "↘" "↙" "↚" "↛" "↮" "⟵" "⟶" "⟷" "⇦" "⇨"
  "⇧" "⇩" "⬄" "⇳" "⬀" "⬁" "⬂" "⬃" "⬅" "⮕" "➡" "⬆" "⬇" "⬈" "⬉" "⬊" "⬋" "⬌"
  "⬍" "🡐" "🡒" "🡑" "🡓" "🡔" "🡕" "🡖" "🡗" "🡘" "🡙" "🠹" "🠸" "🠻" "🠺" "🡆" "🡅" "🡇"
  "🠾" "🠽" "🠿" "🡂" "🡁" "🡃" "🠐" "🠒" "🠑" "🠓" "🠖" "🠕" "🠗" "🠚" "🠙" "🠛" "🠞" "🠝"
  "🠟" "⭠" "⭢" "⭡" "⭣" "⭤" "⭥" "⭦" "⭧" "⭨" "⭩" "🠂" "🠁" "🠃" "🠆" "🠅" "🠇" "🠈"
  "🠊" "🠉" "🠋" "🠢" "🠡" "🠣" "🠦" "🠥" "🠧" "🠪" "🠩" "🠫" "🠮" "🠭" "🠯" "🠲" "🠱" "🠳"
  "🡠" "🡢" "🡡" "🡣" "🡤" "🡥" "🡦" "🡧" "🡪" "🡩" "🡫" "🡬" "🡭" "🡮" "🡯" "🡰" "🡲" "🡱"
  "🡳" "🡴" "🡵" "🡶" "🡷" "🡺" "🡹" "🡻" "🡼" "🡽" "🡾" "🡿" "🢂" "🢁" "🢃" "🢄" "🢅" "🢆"
  "🢇" "⮈" "⮊" "⮉" "⮋" "⮘" "⮙" "⮚" "⮛" "⮜" "⮝" "⮞" "⮟" "➢" "➣" "➤" "⭪" "⭬"
  "⭫" "⭭" " " "⇢" "⇡" "⇣" "⤍" "⤎" "⤏" "⤑" "↼" "⇀" "↽" "⇁" "↿" "↾" "⇃" "⇂"
  "⇌" "⥋" "⥌" "⥍" "⥎" "⥏" "⥐" "⥑" "⥓" "⥔" "⥕" "⥖" "⥗" "⥘" "⥙" "⥛" "⥜" "⥝"
  "⥞" "⥟" "⥠" "⥡" "⥤" "⥣" "⥥" "⥦" "⥨" "⥧" "⥩" "⥮" "⥯" "⥬" "⥫" "⥭" "⮄" "⮆"
  "⮅" "⮇" "⇊" "⇇" "⇉" "⇄" "⇅" "⇵" "⮂" "⮁" "⮃" "⭿" "⇐" "⇒" "⇑" "⇓" "⇔" "⇕"
  "⇖" "⇗" "⇘" "⇙" "⇍" "⇏" "⇎" "⟸" "⟹" "⟺" "⤂" "⤃" "⤄" "⤇" "⬱" "⇶" "⇛" "⤊"
  "⤋" "⭅" "⭆" "⟰" "⟱" "⭰" "⭲" "⭱" "⭳" "⭶" "⭷" "⭹" "⭸" "↦" "↥" "↧" "⇥" "⤒"
  "⤓" "↨" "⟼" "⇜" "⇝" "⟿" "↝" "↭" "⤳" "⇷" "⇸" "⤉" "⤈" "⇹" "⇻" "⇞" "⇟" "⇼"
  "⭼" "⭻" "⭽" "⤀" "⬵" "⤁" "⤔" "⤕" "⤅" "⤖" "⤐" "⤗" "⬽" "⤘" "⮠" "⮡" "⮢" "⮣"
  "⮤" "⮥" "⮦" "⮧" "↱" "↲" "↳" "⬐" "⬎" "⬑" "⬏" "↴" "↵" "⮑" "⮒" "⮓" "⮔" "⤣"
  "⤤" "⤥" "⤦" "↩" "↪" "↫" "↬" "⤙" "⤚" "⤛" "⤜" "⥽" "⥾" "⥿" "⤼" "⤽" "↷" "⤾"
  "⤿" "⤸" "⤹" "⤻" "⭮" "↺" "↻" "⟲" "⟳" "⥀" "⥁" "⮍" "⮎" "⮏" "⮰" "⮱" "⮲" "⮳"
  "⮴" "⮵" "⮶" "⮷" "➥" "➦" "⮩" "⮪" "⮫" "⮬" "⮭" "⮮" "⮯" "➩" "➪" "➫" "➬" "➭"
  "➮" "➯" "➱" "🢡" "🢢" "🢣" "🢤" "🢥" "🢦" "🢧" "🢨" "🢩" "🢪" "🢫" "⇪" "⮸" "⇫" "⇬"
  "⇭" "⇮" "⇯" "➵" "➴" "➶" "➸" "➷" "➹" "➘" "➚" "⇰" "➜" "➔" "➝" "➞" "➟" "➠"
  "➧" "➨" "➻" "➼" "➽" "◄" "►" "◅" "▻" "▶" "▲" "▼" "⯈" "⯅" "⯆" "↞" "↠" "↟"
  "↡" "⯭" "⯮" "⯯" "🠴" "🠶" "🠵" "🠷" "🢚" "🢙" "🢛" "↢" "↣" "⇾" "⇿" "⤝" "⤞" "⤟"
  "⤠" "⤢" "⭎" "⭏" "⭚" "⭛" "⭜" "⭝" "⭞" "⭟" "⤷" "⤴" "⤵" "⥂" "⥃" "⥄" "⭀" "⥱"
  "⥶" "⥸" "⭂" "⭈" "⭊" "⥵" "⭁" "⭇" "⭉" "⥲" "⭋" "⭌" "⥳" "⥴" "⥆" "⥅" "⥹" "⥻"
  "⇴" "⥈" "⬲" "⟴" "⥇" "⭃" "⭄" "⇲" "⥰" "⤪" "⤨" "⤧" "⤩" "⤭" "⤮" "⤯" "⤰" "⤱"
  "⤲" "⤫" "⤬")

;;;###autoload
(helm-snippets-define helm-snippets-star
  "Insert Unicode stars using Helm."
  "⭐" "🌟" "🌠" "✨" "🔯" "☪" "★" "☆" "⚝" "✩" "✪" "✫" "✬" "✭" "🟉" "🟊" "✮" "✯"
  "✰" "⭑" "⭒" "⛤" "⛥" "⛦" "⛧" "🟀" "🟁" "🟂" "🟃" "✦" "✧" "🟄" "🟅" "🟆" "🟇" "🟈"
  "🟋" "🟌" "🟍" "✶" "✡" "❂" "✴" "✵" "✷" "✸" "🟎" "🟏" "🟐" "🟑" "✹" "🟒" "🟓" "🟔"
  "⋆" "⍟" "≛" "⍣" "٭" "*" "⁎" "⁑" "✱" "✲" "✻" "✼" "✽" "❃" "❉" "⧆" "꙳" "﹡"
  "＊" "✢" "✣" "✤" "✥" "🞯" "🞰" "🞱" "🞲" "🞳" "🞴" "🞵" "🞶" "🞷" "🞸" "🞹" "🞺" "🞻"
  "🞼" "🞽" "🞾" "🞿" "✳" "❊" "❋" "✺" "∗" "⊛" "❇" "❈" "❄" "❅" "❆" "✿" "❀" "❁" "✾")

(provide 'helm-snippets)
;;; tokei.el ends here
