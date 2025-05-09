with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions;

with Check_Positive;
with Fibonacci;
with Hello_World;
with Hello_Tasks;
with Int_Sqrt_Contract;
with Show_Predicates;
with Show_Simple_Contract;
with Show_Simple_Postcondition;
with Show_Simple_Precondition;
with Sum_Two_Numbers;
with Set_Diff;
with Quadratic_Solver; 
procedure PCP is
   pragma Assertion_Policy (Check);
   Choice : Character;

   procedure Handle_User_Choice (Choice : Character) is
   begin
      case Choice is
         when '0' =>
            Hello_World;

         when '1' =>
            Check_Positive;

         when '2' =>
            Sum_Two_Numbers;

         when '3' =>
            Show_Simple_Contract;

         when '4' =>
            Show_Simple_Precondition;

         when '5' =>
            Show_Simple_Postcondition;

         when '6' =>
            Int_Sqrt_Contract;

         when '7' =>
            Hello_Tasks;

         when '8' =>
            Show_Predicates;

         when 'A' | 'a' =>
            Fibonacci;
         
         when 'B' | 'b' =>
            Set_Diff;
         
         when 'C' | 'c' =>
            Quadratic_Solver;

         when others =>
            Put_Line ("Invalid choice. Please try again.");

      end case;
   end Handle_User_Choice;
begin
   New_Line;
   loop
      Put_Line ("Select an example to run:");
      New_Line;
      Put_Line ("EXAMPLES:");
      New_Line;
      Put_Line ("0. Hello World");
      Put_Line ("1. Check Positive");
      Put_Line ("2. Sum Two Numbers");
      Put_Line ("3. Show Simple Contract");
      Put_Line ("4. Show Simple Pre Condition");
      Put_Line ("5. Show Simple Post Condition");
      Put_Line ("6. Int Square Root Contract");
      Put_Line ("7. Hello Tasks");
      Put_Line ("8. Show Predicates");
      New_Line;
      Put_Line ("EXERCISES:");
      New_Line;
      Put_Line ("A. Fibonacci (SW03-EX1)");
      Put_Line ("B. Array Difference (SW04-EX2)");
      Put_Line ("C. Quadratic Solver (SW05-EX5)");
      New_Line;
      Put_Line ("Q. Quit");
      New_Line;

      Put ("Enter your choice: ");
      Get (Choice);
      New_Line;
      case Choice is
         when 'Q' | 'q' =>
            Put_Line ("Exiting program. Goodbye!");
            exit;

         when others =>
            begin
               Handle_User_Choice (Choice);
               New_Line;
               Put_Line ("Example Completed!");
            exception
               when E : others =>
                  Put_Line
                    ("An error occurred during Example Execution: "
                     & Ada.Exceptions.Exception_Information (E));
            end;
      end case;

      New_Line;
   end loop;
end PCP;
