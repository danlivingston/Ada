with Ada.Text_IO;       use Ada.Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;
with Ada.Containers.Vectors;
with Ada.Numerics.Elementary_Functions;

procedure Quadratic_Solver is

   use Ada.Numerics.Elementary_Functions;

   package Float_Vector is new
     Ada.Containers.Vectors (Index_Type => Natural, Element_Type => Float);

   use Float_Vector;

   function Discriminant (A, B, C : Float) return Float is
   begin
      return B * B - 4.0 * A * C;
   end Discriminant;

   function Has_Real_Solutions (A, B, C : Float) return Boolean is
   begin
      return Discriminant (A, B, C) >= 0.0;
   end Has_Real_Solutions;

   function Number_Of_Solutions (A, B, C : Float) return Natural is
      D : Float := Discriminant (A, B, C);
   begin
      if D < 0.0 then
         return 0;
      elsif D = 0.0 then
         return 1;
      else
         return 2;
      end if;
   end Number_Of_Solutions;

   function Solve_Quadratic (A, B, C : Float) return Float_Vector.Vector is
      Result : Float_Vector.Vector := Empty_Vector;
      D      : Float := Discriminant (A, B, C);
   begin
      if D < 0.0 then
         return Result;
      elsif D = 0.0 then
         Append (Result, -B / (2.0 * A));
      else
         Append (Result, (-B + Sqrt (D)) / (2.0 * A));
         Append (Result, (-B - Sqrt (D)) / (2.0 * A));
      end if;
      return Result;
   end Solve_Quadratic;

   Solutions : Float_Vector.Vector;
   A         : Float := 1.0;
   B         : Float := -3.0;
   C         : Float := 2.0;

begin
   Put_Line ("Equation: x^2 - 3x + 2 = 0");

   if Has_Real_Solutions (A, B, C) then
      Put_Line ("Has real solutions.");
      Put_Line
        ("Number of solutions: "
         & Natural'Image (Number_Of_Solutions (A, B, C)));
      Solutions := Solve_Quadratic (A, B, C);
      for S of Solutions loop
         Put ("x = ");
         Put (S, Aft => 2, Exp => 0);
         New_Line;
      end loop;
   else
      Put_Line ("No real solutions.");
   end if;
end Quadratic_Solver;
