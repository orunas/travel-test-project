(ns test-project.task-test
  (:require
    [clojure.test :refer :all]
    [test-project.task :as t]))

(def datetime1 (java.time.ZonedDateTime/parse "2017-10-16T00:00:01.390Z") )

(deftest
  task-test
  (is
    (=
      (list {:task :update-data-task,
        :method :update-data-method,
        :unprocessed-body [true ['(print "This is update-data-method")]],
        :?airline {:type :uri, :value "W6", :prefix-ns "http://travelplanning.ex/Airline/"},
        :?beginDate {:type :literal,
                     :datatype "http://www.w3.org/2001/XMLSchema#dateTime",
                     :value datetime1}})
      (t/get-task-instance
        {:update-data-method {:name :update-data-method,
                              :task '(:update-data-task ?airline ?beginDate)
                              :precondition nil
                              :body '[true [(print "This is update-data-method")]]}}
        #(first %)
        (rest (t/add-task
           :update-data-task
           {:type :uri, :value "W6", :prefix-ns "http://travelplanning.ex/Airline/"}
           {:type     :literal,
            :datatype "http://www.w3.org/2001/XMLSchema#dateTime",
            :value    datetime1}))))))



