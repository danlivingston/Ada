with Ada.Text_IO; use Ada.Text_IO;

procedure Hello_Tasks is

   task Greeter_A;
   task body Greeter_A is
   begin
      for I in 1 .. 3 loop
         Put_Line ("Hello from Task A - round " & I'Image);
         delay 0.5;
      end loop;
   end Greeter_A;

   task Greeter_B;
   task body Greeter_B is
   begin
      for I in 1 .. 3 loop
         Put_Line ("Hello from Task B - round " & I'Image);
         delay 0.5;
      end loop;
   end Greeter_B;

begin
   Put_Line ("Main program started.");
   delay 2.0;
   Put_Line ("Main program finished.");
end Hello_Tasks;
