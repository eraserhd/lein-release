(ns leiningen.release
  (:require
   [clojure.java.shell :as sh]
   [clojure.string     :as string])
  (:import
   [java.util.regex Pattern]))

(defn raise [fmt & args]
  (throw (RuntimeException. (apply format fmt args))))

(def default-clojars-url "clojars@clojars.org:")

(def ^:dynamic config {:tag-format [:project-name "-" :version],
                       :clojars-url default-clojars-url})

(def scm-systems
     {:git {:add    ["git" "add"]
            :tag    ["git" "tag"]
            :commit ["git" "commit"]
            :push   ["git" "push" "origin" "master"]
            :status ["git" "status"]}})

(defn detect-scm []
  (or
   (:scm config)
   (cond
     (.exists (java.io.File. ".git"))
     :git
     :no-scm-detected
     (raise "Error: no scm detected! (I know only about git for now)."))))

(defn sh! [& args]
  (let [res (apply sh/sh args)]
    (.println System/out (:out res))
    (.println System/err (:err res))
    (when-not (zero? (:exit res))
      (raise "Error: command failed %s => %s" args res))))

(defn scm! [cmd & args]
  (let [scm   (detect-scm)
        scm-cmd (get-in scm-systems [scm cmd])]
    (if-not scm-cmd
      (raise "No such SCM command: %s in %s" cmd scm))
    (apply sh! (concat scm-cmd args))))

(def maven-version-regexes
     {:major-only                               #"(\d+)(?:-(.+))?"
      :major-and-minor                          #"(\d+)\.(\d+)(?:-(.+))?"
      :major-minor-and-incremental              #"(\d+)\.(\d+)\.(\d+)(?:-(.+))?"})

(defn parse-maven-version [vstr]
  ;; <MajorVersion [> . <MinorVersion [> . <IncrementalVersion ] ] [> - <BuildNumber | Qualifier ]>
  (cond
    (re-matches (:major-only maven-version-regexes) vstr)
    (let [[[_ major qualifier]] (re-seq (:major-only maven-version-regexes) vstr)]
      {:format      :major-only
       :major       major
       :minor       nil
       :incremental nil
       :qualifier   qualifier})

    (re-matches (:major-and-minor maven-version-regexes) vstr)
    (let [[[_ major minor qualifier]] (re-seq (:major-and-minor maven-version-regexes) vstr)]
      {:format      :major-and-minor
       :major       major
       :minor       minor
       :incremental nil
       :qualifier   qualifier})

    (re-matches (:major-minor-and-incremental maven-version-regexes) vstr)
    (let [[[_ major minor incremental qualifier]] (re-seq (:major-minor-and-incremental maven-version-regexes) vstr)]
      {:format      :major-minor-and-incremental
       :major       major
       :minor       minor
       :incremental incremental
       :qualifier   qualifier})
    :else
    {:format :not-recognized
     :major vstr}))

(comment

  (parse-maven-version "1")
  {:format :major-only, :major "1", :minor nil, :incremental nil, :qualifier nil}
  (parse-maven-version "1-SNAPSHOT")
  {:format :major-only, :major "1", :minor nil, :incremental nil, :qualifier "SNAPSHOT"}
  (parse-maven-version "1-b123")
  {:format :major-only, :major "1", :minor nil, :incremental nil, :qualifier "b123"}

  (parse-maven-version "1.2")
  {:format :major-and-minor, :major "1", :minor "2", :incremental nil, :qualifier nil}
  (parse-maven-version "1.2-SNAPSHOT")
  {:format :major-and-minor, :major "1", :minor "2", :incremental nil, :qualifier "SNAPSHOT"}
  (parse-maven-version "1.2-b123")
  {:format :major-and-minor, :major "1", :minor "2", :incremental nil, :qualifier "b123"}

  (parse-maven-version "1.2.3")
  {:format :major-minor-and-incremental, :major "1", :minor "2", :incremental "3", :qualifier nil}
  (parse-maven-version "1.2.3-SNAPSHOT")
  {:format :major-minor-and-incremental, :major "1", :minor "2", :incremental "3", :qualifier "SNAPSHOT"}
  (parse-maven-version "1.2.3-b123")
  {:format :major-minor-and-incremental, :major "1", :minor "2", :incremental "3", :qualifier "b123"}

  (parse-maven-version "1.2.3-rc1")
  {:format :major-minor-and-incremental, :major "1", :minor "2", :incremental "3", :qualifier "rc1"}

  )

(defn ^:dynamic get-release-qualifier []
  (System/getenv "RELEASE_QUALIFIER"))


;; 1.0.116-SNAPSHOT
;; 1.0.116-v2

;; See: http://mojo.codehaus.org/versions-maven-plugin/version-rules.html
(defn compute-next-development-version [current-version]
  (let [parts             (vec (.split current-version "\\."))
        version-parts     (vec (take (dec (count parts)) parts))
        minor-version     (last parts)
        new-minor-version (str (inc (Integer/parseInt minor-version)) "-SNAPSHOT")]
    (string/join "." (conj version-parts new-minor-version))))



(defn replace-project-version [old-vstring new-vstring]
  (let [proj-file     (slurp "project.clj")
        new-proj-file (.replaceAll proj-file (format "\\(defproject .+? %s" old-vstring) new-vstring )
        matcher       (.matcher
                       (Pattern/compile (format "(\\(defproject .+? )\"\\Q%s\\E\"" old-vstring))
                       proj-file)]
    (if-not (.find matcher)
      (raise "Error: unable to find version string %s in project.clj file!" old-vstring))
    (.replaceFirst matcher (format "%s\"%s\"" (.group matcher 1) new-vstring))))

(defn set-project-version! [old-vstring new-vstring]
  (spit "project.clj" (replace-project-version old-vstring new-vstring)))

(defn detect-deployment-strategy [project]
  (cond
    (:deploy-via config)
    (:deploy-via config)

    (:repositories project)
    :lein-deploy

    :no-deploy-strategy
    :lein-install))


(defn clojars-url []
  (or (:clojars-url config)
      default-clojars-url))

(defn perform-deploy! [project project-jar]
  (case (detect-deployment-strategy project)

    :lein-deploy
    (sh! "lein" "deploy")

    :lein-install
    (sh! "lein" "install")

    :clojars
    (sh! "scp" "pom.xml" project-jar (clojars-url))

    :shell
    (apply sh! (:shell config))

    (raise "Error: unrecognized deploy strategy: %s" (detect-deployment-strategy))))

(defn extract-project-version-from-file
  ([]
     (extract-project-version-from-file "project.clj"))
  ([proj-file]
     (let [s (slurp proj-file)
           m (.matcher (Pattern/compile "\\(defproject .+? \"([^\"]+?)\"") s)]
       (if-not (.find m)
         (raise "Error: unable to find project version in file: %s" proj-file))
       (.group m 1))))

(defn is-snapshot? [vstring]
  (.endsWith vstring "-SNAPSHOT"))

(defn compute-release-version [current-version]
  (str (.replaceAll current-version "-SNAPSHOT" "")
       (get-release-qualifier)))

(comment

  (binding [get-release-qualifier (fn [] "-v2")]
    (compute-release-version "1.0.116-SNAPSHOT"))

)

(defn compute-tag-name
  [config values]
  (->> (:tag-format config)
       (map (fn [tag-name-part]
              (if (keyword? tag-name-part)
                (tag-name-part values)
                tag-name-part)))
       (apply str)))

(defn release [project & args]
  (binding [config (merge config (or (:lein-release project) {}))]
    (let [current-version  (get project :version)
          release-version  (compute-release-version current-version)
          next-dev-version (compute-next-development-version (.replaceAll current-version "-SNAPSHOT" ""))
          target-dir       (:target-path project (:target-dir project (:jar-dir project "."))) ; target-path for lein2, target-dir or jar-dir for lein1
          jar-file-name    (format "%s/%s-%s.jar" target-dir (:name project) release-version)]
      (when (is-snapshot? current-version)
        (println (format "setting project version %s => %s" current-version release-version))
        (set-project-version! current-version release-version)
        (println "adding, committing and tagging project.clj")
        (scm! :add "project.clj")
        (scm! :commit "-m" (format "lein-release plugin: preparing %s release" release-version))
        (scm! :tag (compute-tag-name config {:project-name (:name project)
                                             :version release-version})))
      (when-not (.exists (java.io.File. jar-file-name))
        (println "creating jar and pom files...")
        (sh! "lein" "jar")
        (sh! "lein" "pom"))
      (perform-deploy! project jar-file-name)
      (when-not (is-snapshot? (extract-project-version-from-file))
        (println (format "updating version %s => %s for next dev cycle" release-version next-dev-version))
        (set-project-version! release-version next-dev-version)
        (scm! :add "project.clj")
        (scm! :commit "-m" (format "lein-release plugin: bumped version from %s to %s for next development cycle" release-version next-dev-version))))))

