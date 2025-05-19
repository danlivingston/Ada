with Ada.Text_IO; use Ada.Text_IO;
with Ada.Calendar; use Ada.Calendar;

procedure RendezVousExample is

   -- Task Server that can receive messages via rendezvous
   task Server is
      entry Print_Message(Msg : String);
   end Server;

   task body Server is
   begin
      loop
         select
            -- Rendezvous: the server accepts a call from a client
            accept Print_Message(Msg : String) do
               Put_Line("Server received: " & Msg);
            end Print_Message;

         or
            -- Timeout: exit if no messages are received in 5 seconds
            delay 5.0;
            Put_Line("No request received in 5 seconds. Exiting...");
            exit;

         end select;
      end loop;
   end Server;

   -- First client task
   task Client_1;
   task body Client_1 is
   begin
      delay 1.0;
      Server.Print_Message("Hello from Client 1");
      delay 2.0;
      Server.Print_Message("Another from Client 1");
   end Client_1;

   -- Second client task
   task Client_2;
   task body Client_2 is
   begin
      delay 0.5;
      Server.Print_Message("Hello from Client 2");
      delay 3.0;
      Server.Print_Message("Another from Client 2");
   end Client_2;

begin
   -- Main task does nothing, just lets the other tasks run
   null;
end RendezVousExample;