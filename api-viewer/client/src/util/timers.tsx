/** Sleeps for a while */
export function sleep(millis: number): Promise<void> {
  return new Promise<void>((resolve) => {
    setTimeout(resolve, millis);
  });
}

// Wrap a callback fn, so that, as long as the wrapper continues to be
// invoked, it will not be triggered. The callback will be called
// after the wrapper stops being called for delayMs milliseconds.
export function debounce<T>(delayMs: number, fn: (v: T) => void): (v: T) => void {
  let callScheduled: number | null = null;

  function delayedCall(newValue: T) {
    if (callScheduled != null) {
      window.clearTimeout(callScheduled);
    }
    callScheduled = window.setTimeout(() => fn(newValue), delayMs);
  }

  return delayedCall;
}
