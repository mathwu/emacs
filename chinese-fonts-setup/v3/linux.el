;;; `cfs--custom-set-fontsnames' 列表有3个子列表，第1个为英文字体列表，第2个为中文字体列表，
;;; 第3个列表中的字体用于显示不常用汉字，每一个字体列表中，*第一个* *有效并可用* 的字体将被使用。
;;; 将光标移动到上述列表中，按 `C-c C-c' 可以测试字体显示效果。另外，用户可以通过命令
;;; `cfs-insert-fontname’ 来选择一个 *可用* 字体，然后在当前光标处插入其字体名称。
(setq cfs--custom-set-fontnames
      '(
        ("Inconsolata" "Monaco" "Inziu Iosevka TC" "Inziu Iosevka CL" "DejaVu Sans Mono")
        ("微软雅黑" "Inziu Iosevka Slab TC" "Hiragino Sans GB" "Inziu Roboto TC" "Inziu Iosevka TC" "Inziu Iosevka CL" "微软雅黑" "Microsoft Yahei" "Microsoft_Yahei")
        ("HanaMinB")
       ))

;;; `cfs--custom-set-fontsizes' 中，所有元素的结构都类似：(英文字号 中文字号 EXT-B字体字号)
;;; 将光标移动到各个数字上，按 C-c C-c 查看光标处字号的对齐效果。
;;; 按 C-<up> 增大光标处字号，按 C-<down> 减小光标处字号。
(setq cfs--custom-set-fontsizes
      '(
        (9    10 10)
        (10   11 11)
        (11.5 12.5 12.5)
        (12.5 13.5 13.5)
        (14   15 15)
        (16   17 17)
        (18   18.5 18.5)
        (20   21 21)
        (22   23 23)
       ))
