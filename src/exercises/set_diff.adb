with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;

procedure Set_Diff is

   package String_Vector is new Ada.Containers.Vectors
     (Index_Type   => Natural,
      Element_Type => Unbounded_String);

   use String_Vector;
   use Ada.Strings.Unbounded;

   function Member(S : Unbounded_String; V : Vector) return Boolean is
   begin
      for E of V loop
         if E = S then
            return True;
         end if;
      end loop;
      return False;
   end Member;

   function Set_Difference(Set1, Set2 : Vector) return Vector is
      Result : Vector := Empty_Vector;
   begin
      for E of Set1 loop
         if not Member(E, Set2) then
            Append(Result, E);
         end if;
      end loop;
      return Result;
   end Set_Difference;

   Set1 : Vector := Empty_Vector;
   Set2 : Vector := Empty_Vector;
   Diff : Vector;

begin
   Append(Set1, To_Unbounded_String("a"));
   Append(Set1, To_Unbounded_String("b"));
   Append(Set1, To_Unbounded_String("c"));
   Append(Set1, To_Unbounded_String("d"));

   Append(Set2, To_Unbounded_String("b"));
   Append(Set2, To_Unbounded_String("d"));
   Append(Set2, To_Unbounded_String("e"));
   Append(Set2, To_Unbounded_String("f"));

   Diff := Set_Difference(Set1, Set2);

   Put("Difference Between two Arrays: ");
   for E of Diff loop
      Put(To_String(E) & " ");
   end loop;
   New_Line;
end Set_Diff;