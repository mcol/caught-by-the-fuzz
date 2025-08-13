# summary

    Code
      summary(res)
    Message
      Fuzzed 1 function on 2 inputs:
    Output
            
             FAIL WARN SKIP OK
        list    0    0    0  2
      
      [ FAIL 0 | WARN 0 | SKIP 0 | OK 2 ]

---

    Code
      summary(res, tabulate = FALSE)
    Message
      Fuzzed 1 function on 2 inputs:
    Output
      
      [ FAIL 0 | WARN 0 | SKIP 0 | OK 2 ]

# print

    Code
      print(res)
    Message
      v  🏃 You didn't get caught by the fuzz!
    Output
      
       [ FAIL 0 | WARN 0 | SKIP 0 | OK 2 ] 

---

    Code
      print(res, show_all = TRUE)
    Message
      v  🏃 You didn't get caught by the fuzz!
      
      -- Test input: NA 
    Output
         list    OK  
       median    OK  
      
       [ FAIL 0 | WARN 0 | SKIP 0 | OK 2 ] 

