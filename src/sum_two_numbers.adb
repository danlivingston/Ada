with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Sum_Two_Numbers is

   procedure Add (A, B : Integer; Result : out Integer) is
   begin
      Result := A + B;
   end Add;

   X, Y, Z : Integer;

begin
   Put ("Enter first number: ");
   Get (X);
   Put ("Enter second number: ");
   Get (Y);

   Add (X, Y, Z);

   Put ("The sum is: ");
   Put (Z);
   New_Line;
end Sum_Two_Numbers;
