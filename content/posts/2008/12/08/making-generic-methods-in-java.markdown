Generics in Java increase type safety and often make code more readable by removing explicit type casting.  Defining a class using generics provides a type template mechanism that can make an implementation useful across multiple, possibly not yet known, types.

What I always forget, however, is that you can also write generic methods (in particular, the syntax doesn't seem to stick in my head).  Generic methods make are very nice for writing all sorts of helper methods in Java.  Here's an example helper for a JUnit test to assert that two lists are disjoint:

<quickcode:noclick>
private <T> void assertListsDisjoint(List<T> a, List<T> b) {
  for (T t : b) {
    assertFalse(t.toString(), a.contains(t));
  }
}
</quickcode>
