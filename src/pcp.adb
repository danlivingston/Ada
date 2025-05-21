with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions;

with Declarative_Regions;
with Design_By_Contract;
with In_And_Out;
with RendezVous;
with Protected_Objects_Types;
with Fibonacci;
with Set_Diff;
with Quadratic_Solver;

procedure PCP is
   pragma Assertion_Policy (Check);
   Choice : Character;

   procedure Handle_User_Choice (Choice : Character) is
   begin
      case Choice is
         when '1' =>
            Declarative_Regions;

         when '2' =>
            In_And_Out;

         when '3' =>
            Design_By_Contract;

         when '4' =>
            RendezVous;

         when '5' =>
            Protected_Objects_Types;

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
      Put_Line ("Select a program to run:");
      New_Line;
      Put_Line ("Focus Points:");
      New_Line;
      Put_Line ("1. Declarative regions");
      Put_Line ("2. In & Out Parameters");
      Put_Line ("3. Design by Contract");
      Put_Line ("4. Rendezvous");
      Put_Line ("5. Protected Objects and Types");
      New_Line;
      Put_Line ("Exercises:");
      New_Line;
      Put_Line ("A. Fibonacci (SW03-EX1)");
      Put_Line ("B. Array Difference (SW04-EX2)");
      Put_Line ("C. Quadratic Solver (SW05-EX5)");
      New_Line;
      Put_Line ("Q. Quit");
      New_Line;

      -- Gets one character input (text longer than 1 will be looped over one by one)
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
