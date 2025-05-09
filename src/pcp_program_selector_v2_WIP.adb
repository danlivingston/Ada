-- filepath: c:\development\PCP\Ada\pcp\src\pcp.adb
with Ada.Text_IO; use Ada.Text_IO;
with Hello_World;
with Check_Positive;
with Show_Simple_Contract;

procedure PCP is
   -- Define a procedure access type
   type Procedure_Access is access procedure;

   -- Define a record to hold menu entries
   type Menu_Entry is record
      Name   : String (1 .. 50); -- Fixed-length string
      Action : Procedure_Access;
   end record;

   -- Define the menu as an array of Menu_Entry
   Menu : constant array (1 .. 3) of Menu_Entry :=
     ((Name => "Hello World", Action => Hello_World'Access),
      (Name => "Check Positive", Action => Check_Positive'Access),
      (Name => "Show Simple Contract", Action => Show_Simple_Contract'Access));

   Choice : Integer;
begin
   loop
      -- Display menu
      Put_Line ("Select an example to run:");
      for I in Menu'Range loop
         Put_Line (I'Image & ". " & Menu (I).Name);
      end loop;
      Put_Line ("0. Quit");
      New_Line;

      -- Get user input
      Put ("Enter your choice: ");
      Ada.Text_IO.Get (Choice); -- Correctly pass the `Choice` variable
      New_Line;

      -- Handle user choice
      if Choice = 0 then
         Put_Line ("Exiting program. Goodbye!");
         exit;
      elsif Choice in Menu'Range then
         Menu (Choice).Action.all; -- Call the associated procedure
      else
         Put_Line ("Invalid choice. Please try again.");
      end if;

      New_Line;
   end loop;
end PCP;
