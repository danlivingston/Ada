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
            Fibonacci;

         when '4' =>
            Show_Simple_Contract;

         when '5' =>
            Show_Simple_Precondition;

         when '6' =>
            Show_Simple_Postcondition;

         when '7' =>
            Int_Sqrt_Contract;

         when '8' =>
            Hello_Tasks;

         when '9' =>
            Show_Predicates;

         when others =>
            Put_Line ("Invalid choice. Please try again.");
      end case;
   end Handle_User_Choice;
begin
   New_Line;
   loop
      Put_Line ("Select an example to run:");
      Put_Line ("0. Hello World");
      Put_Line ("1. Check Positive");
      Put_Line ("2. Sum Two Numbers");
      Put_Line ("3. Fibonacci");
      Put_Line ("4. Show Simple Contract");
      Put_Line ("5. Show Simple Pre Condition");
      Put_Line ("6. Show Simple Post Condition");
      Put_Line ("7. Int Square Root Contract");
      Put_Line ("8. Hello Tasks");
      Put_Line ("9. Show Predicates");
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
