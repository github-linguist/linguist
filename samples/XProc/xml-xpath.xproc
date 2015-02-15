<p:pipeline xmlns:p="http://www.w3.org/ns/xproc"
  name="one-two-three"
  version="1.0">
  <p:identity>
    <p:input port="source">
      <p:inline>
        <root>
          <first/>
          <prices/>
          <names/>
        </root>
      </p:inline>
    </p:input>
  </p:identity>
  <p:insert match="/root/first" position="first-child">
    <p:input port="insertion" select="(//item)[1]">
      <p:pipe port="source" step="one-two-three"/>
    </p:input>
  </p:insert>
  <p:insert match="/root/prices" position="first-child">
    <p:input port="insertion" select="//price">
      <p:pipe port="source" step="one-two-three"/>
    </p:input>
  </p:insert>
  <p:insert match="/root/names" position="first-child">
    <p:input port="insertion" select="//name">
      <p:pipe port="source" step="one-two-three"/>
    </p:input>
  </p:insert>
</p:pipeline>
