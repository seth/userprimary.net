Yesterday I was seeing intermittent build failures for a Maven2 project resulting from the JVM running out of memory while trying to <em>compile the code</em>.  A sign of project bloat?  Yes.  But also the sign of a silly build tool gone wrong.

The fix was to add some XML config to the project's pom.xml file:

<pre>    &lt;plugins&gt;
      &lt;plugin&gt;
        &lt;groupId&gt;org.apache.maven.plugins&lt;/groupId&gt;
        &lt;artifactId&gt;maven-compiler-plugin&lt;/artifactId&gt;
        &lt;configuration&gt;
          &lt;fork&gt;true&lt;/fork&gt;
          &lt;meminitial&gt;128m&lt;/meminitial&gt;
          &lt;maxmem&gt;512m&lt;/maxmem&gt;
        &lt;/configuration&gt;
      &lt;/plugin&gt;
    &lt;/plugins&gt;

I feel more productive already.</pre>
<!-- technorati tags start --><p style="text-align:right;font-size:10px;">Technorati Tags: <a href="http://www.technorati.com/tag/programming" rel="tag">programming</a>, <a href="http://www.technorati.com/tag/java" rel="tag">java</a></p><!-- technorati tags end -->
