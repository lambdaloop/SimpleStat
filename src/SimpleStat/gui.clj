;(ns SimpleStat.gui)

(import '(javax.swing JFrame JPanel JButton
		      JComboBox JFileChooser JOptionPane)
        '(java.awt GridBagLayout GridBagConstraints Insets))
(use '(clojure.contrib
       [swing-utils :only (add-action-listener)])
     '(SimpleStat stats))


(defmacro set-grid! [constraints field value]
  `(set! (. ~constraints ~(symbol (name field)))
         ~(if (keyword? value)
            `(. java.awt.GridBagConstraints
                ~(symbol (name value)))
            value)))

(defmacro set-grid-many! [constraints & body]
  (cons `do
	(map
	 #(concat `(set-grid! ~constraints) %)
	 (partition 2 body))))
  
#_(grid-bag-layout container
		   (:weightx 0.5 :weighty .6) ;default options
		   (comp-one :gridx 0, :gridy 0)
		   (come-two :gridx :RELATIVE, :gridwidth 2)
		   ;; ... more components & constraints ...
		   )


(defmacro set-prop-gui!
  [constraints object properties]
  (cons
   `do
   (for [[prop val] properties]
     (if (= prop :listener)
       `(add-action-listener ~object ~val)
       `(set-grid! ~constraints ~prop ~val)))))
    

(defmacro grid-bag-layout [container default & comps]
  (concat
   `(do (. ~container (setLayout (new GridBagLayout)))) ;use GridBagLayout
   (for [comp comps]
     `(let [c# (new GridBagConstraints)
	    obj# ~(first comp)]
	  (set-prop-gui! c# obj# ~(apply hash-map default))
	  (set-prop-gui! c# obj# ~(apply hash-map (next comp)))
	  (. ~container (add obj# c#))))
   (list `(. ~container validate))))


(defn dialog-box [message]
  (JOptionPane/showMessageDialog
   nil
   message))

(defn printable-map [hmap]
  (str "<html>"
       (apply
	str
	(for [[key val] hmap]
	  (str (name key) ":" \tab val "<br>")))
       "</html>"))
		    

(def frame)
(def data)
(def test-selected)

(defn import-data
  [event]
  (let [file (.getSelectedFile (doto (JFileChooser.)
				 (.showOpenDialog frame)))]
    (if file
      (dialog-box (slurp file)))))
      

(defn test-data
  [event]
  (let [selection (deref *test-selected*)]
    (dialog-box
     (printable-map (apply
	   (case selection
		 "2 Sample T Test" two-sample-t-test
		 "ANOVA"  ANOVA)
	   data)))))

(defn select-test
  [event]
  (set!  test-selected
	  (.. event getSource getSelectedItem))
  (dialog-box (.. event getSource getSelectedItem) " selected!"))

(defn show-gui []
  (binding [frame (JFrame. "Test Data!")
	    data '((1 2 3) (4 5 6 7))
	    test-selected "2 Sample T Test"]
    
    
    (grid-bag-layout (. frame getContentPane)
		     [:weightx 0.5]
		     [(JButton. "Import")
		      :listener import-data
		      :gridwidth 2 :anchor :PAGE_START
		      :gridx 0 :gridy 0]
		     [(JComboBox. (into-array
				   (list "2 Sample T Test" "ANOVA")))
		      :listener select-test
		      :gridx 0, :gridy 1]
		     [(JButton. "TEST!")
		      :listener test-data
		      :gridx 1, :gridy 1])
    (. frame (pack))
    (. frame (setVisible true))))


(SwingUtilites/invokeLater show-gui)
