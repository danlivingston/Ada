with Ada.Text_IO;           use Ada.Text_IO;

procedure Declarative_Regions is
   X : Integer := 10;

   procedure Inner_Procedure is
      -- X is visible here, Y is local to Inner_Procedure
      Y : Integer := X + 5;
   begin
      Put_Line ("Y = " & Integer'Image (Y));
   end Inner_Procedure;

begin
   Inner_Procedure;
end Declarative_Regions;
