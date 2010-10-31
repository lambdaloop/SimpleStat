;(ns SimpleStat.gui)

(import '(javax.swing JFrame JPanel JButton
		      JComboBox JFileChooser JOptionPane
		      SwingUtilities JTextArea)
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


(defn dialog-box [& message]
  (JOptionPane/showMessageDialog
   nil
   (apply str message)))


(defn printable-map [hmap]
  (apply
   str
   (for [[key val] hmap]
     (str (name key) ":  "  val \newline))))
    
		    

(def frame (ref nil))
(def text-area (ref nil))
(def data (ref '((1 2 3) (4 5 6 7))))
(def test-selected (ref "2 Sample T Test"))


(defn show-message [& message]
  (. @text-area (setText (apply str message))))
   
(defn import-data
  [event]
  (if-let [file (.getSelectedFile (doto (JFileChooser.)
				 (.showOpenDialog nil)))]
    (dosync (ref-set data (load-string (slurp file)))
	    (dialog-box @data))))
      

(defn test-data
  [event]
  (show-message
   (printable-map (apply
		   (case @test-selected
			 "2 Sample T Test" two-sample-t-test
			 "ANOVA"  ANOVA)
		   @data))))

(defn select-test
  [event]
  (dosync
   (ref-set test-selected
	    (.. event getSource getSelectedItem))))

(defn show-gui []
  (dosync (ref-set frame (JFrame. "Test Data!"))
	  #_(ref-set data '((1 2 3) (4 5 6 7)))
	  #_(ref-set test-selected "2 Sample T Test")
	  (ref-set text-area (doto (JTextArea.)
			       (.setLineWrap true)
			       (.setWrapStyleWord true)
			       (.setEditable false)
			       (.setText "Data will appear here\n\n\n"))))
	  (grid-bag-layout (. @frame getContentPane)
			   [:fill :HORIZONTAL
			    :weighty 1]
			   [(JButton. "TEST!")
			    :listener test-data
			    :gridx 1, :gridy 1]
			   [(JButton. "Import")
			    :listener import-data
			    :gridwidth 2 
			    :gridx 0 :gridy 0
			    :anchor :NORTH]
			   [(JComboBox. (into-array
					 (list "2 Sample T Test" "ANOVA")))
			    :listener select-test
			    :gridx 0, :gridy 1]
			   [@text-area
			    :gridx 0, :gridy 2
			    :gridwidth 2 :gridheight 1
			    :fill :BOTH
			    :weighty 0.1
			    :insets (Insets. 5 5 5 5)])
	  (doto @frame
	    (.pack)
	    (.setDefaultCloseOperation JFrame/DISPOSE_ON_CLOSE)
	    (.setSize 200 150)
	    (.setVisible true)
	    (.validate)))


(SwingUtilities/invokeLater show-gui)
  
;(. (Thread. show-gui) start)