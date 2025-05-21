import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

public class ProtectedObjectsTypes {

    // In Java, managing shared data between threads typically involves using:
    //
    // - synchronized methods or blocks,
    // - explicit locks (ReentrantLock),
    // - or low-level primitives like wait() and notify().
    //
    // To simplify concurrent access to numeric variables, Java also provides atomic
    // classes such as AtomicInteger and AtomicBoolean,
    // which allow safe updates without locks — but only for very specific use
    // cases.

    // Example of using synchronized methods for thread-safe access
    class SynchronizedCounter {
        private int count = 0;

        // Increment the counter in a thread-safe manner
        public synchronized void increment() {
            count++;
        }

        // Get the current value of the counter in a thread-safe manner
        public synchronized int get() {
            return count;
        }
    }

    // This ensures mutual exclusion, but puts the responsibility for correctness on
    // the programmer.
    // There’s no compiler help for complex coordination.

    // Example of using AtomicInteger for thread-safe access
    class AtomicCounter {
        private AtomicInteger count = new AtomicInteger(0);

        // Increment the counter atomically
        public void increment() {
            count.incrementAndGet();
        }

        // Get the current value of the counter atomically
        public int get() {
            return count.get();
        }
    }

    // This is more efficient than synchronized, but only supports simple
    // operations.

    // Example of using explicit locks for thread-safe access
    class LockBasedCounter {
        private int count = 0;
        private final Lock lock = new ReentrantLock();

        // Increment the counter using a lock
        public void increment() {
            lock.lock();
            try {
                count++;
            } finally {
                lock.unlock();
            }
        }

        // Get the current value of the counter using a lock
        public int get() {
            lock.lock();
            try {
                return count;
            } finally {
                lock.unlock();
            }
        }
    }
}
