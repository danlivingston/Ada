public class RendezVous {

    // In Java, communication and synchronization between threads is typically done using:
    //
    // - synchronized methods or blocks,
    // - shared variables with manual locking (ReentrantLock, volatile),
    // - low-level primitives like wait() and notify().
    //
    // While powerful, these mechanisms are error-prone and require the programmer to manually ensure synchronization and mutual exclusion.

    static class Server {
        // Synchronized method to ensure mutual exclusion
        public synchronized void printMessage(String msg) {
            System.out.println("Server received: " + msg);
        }
    }

    static class Client extends Thread {
        private final Server server;
        private final String message;

        public Client(Server server, String message) {
            this.server = server;
            this.message = message;
        }

        @Override
        public void run() {
            // Call the synchronized method on the server
            server.printMessage(message);
        }
    }

    public static void main(String[] args) {
        Server server = new Server();

        // Create two client threads
        Client client1 = new Client(server, "Hello from Client 1");
        Client client2 = new Client(server, "Hello from Client 2");

        // Start the client threads
        client1.start();
        client2.start();

        // Wait for both clients to finish
        try {
            client1.join();
            client2.join();
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
        }

        System.out.println("All clients have finished.");
    }
}
