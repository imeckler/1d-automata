1D Cellular Automata
================
This uses infinite lists to represent the state of the automaton and
to ensure that the state is not incorrectly truncated until it is printed.


usage:

    $ ./Automata rule width initial-state

example:

    $ ./Automata 90 20 X | head -n 32

              X          
             X X         
            X   X        
           X X X X       
          X       X      
         X X     X X     
        X   X   X   X    
       X X X X X X X X   
      X               X  
     X X             X X 
    X   X           X   X
     X X X         X X X 
          X       X      
         X X     X X     
    X   X   X   X   X   X
     X X X X X X X X X X 
                         
                         
                         
                         
                         
                         
    X                   X
     X                 X 
      X               X  
     X X             X X 
    X   X           X   X
     X X X         X X X 
          X       X      
         X X     X X     
    X   X   X   X   X   X
     X X X X X X X X X X 
