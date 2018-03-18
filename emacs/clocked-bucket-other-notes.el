;;; dynamic dispatching
;; http://nullprogram.com/blog/2018/02/14/

(cl-defgeneric pct-str (a))

(cl-defmethod pct-str ((x clocked)) (clocked-bucket-percentage-str (clocked-percentage x)))

(cl-defmethod pct-str ((x task)) (pct-str (task-clocked x)))

(cl-defmethod pct-str ((x context-tree)) (pct-str (context-tree-clocked x)))

(cl-defmethod pct-str ((x task-tree)) (pct-str (task-tree-clocked x)))

(pct-str (clocked-create :percentage 10))
(pct-str (task-create :name "foo"
                      :clocked (clocked-create :percentage 10)))


;;
;; tt clucket summary shepherd
