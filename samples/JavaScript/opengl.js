<html style="margin: 0;">
  <head>
    <title>Minimal WebGL Example</title>
    <!-- This use of <script> elements is so that we can have multiline text
         without quoting it inside of JavaScript; the web browser doesn't
         actually do anything besides store the text of these. -->
    <script id="shader-fs" type="x-shader/x-fragment">
      precision highp float;
      varying vec4 v_color;
      void main(void) {
        // "Varying" variables are implicitly interpolated across triangles.
        gl_FragColor = v_color;
      }
    </script>
    <script id="shader-vs" type="x-shader/x-vertex">
      attribute vec3 a_position;
      attribute vec4 a_color;
      varying vec4 v_color;
      void main(void) {
        gl_Position = vec4(a_position, 1.0);
        v_color = a_color;
      }
    </script>
    <script type="text/javascript">
      function getShader(gl, id) {
        var scriptElement = document.getElementById(id);
        // Create shader object
        var shader;
        if (scriptElement.type == "x-shader/x-fragment")
          shader = gl.createShader(gl.FRAGMENT_SHADER);
        else if (scriptElement.type == "x-shader/x-vertex")
          shader = gl.createShader(gl.VERTEX_SHADER);
        else
          throw new Error("unknown shader script type");
        // Compile shader from source
        gl.shaderSource(shader, scriptElement.textContent);
        gl.compileShader(shader);
        if (!gl.getShaderParameter(shader, gl.COMPILE_STATUS))
          throw new Error(gl.getShaderInfoLog(shader));
        return shader;
      }
    </script>
  </head>
  <body style="margin: 0;">
    <canvas id="glcanvas" style="border: none; margin: auto; display: block;" width="640" height="480"></canvas>
    <script type="text/javascript">
      var canvas = document.getElementById("glcanvas");

      // Get WebGL context.
      var gl = canvas.getContext("webgl")
            || canvas.getContext("experimental-webgl");
      if (!gl)
        throw new Error("WebGL context not found");

      // Create shader program from vertex and fragment shader code.
      var shaderProgram = gl.createProgram();
      gl.attachShader(shaderProgram, getShader(gl, "shader-vs"));
      gl.attachShader(shaderProgram, getShader(gl, "shader-fs"));
      gl.linkProgram(shaderProgram);
      if (!gl.getProgramParameter(shaderProgram, gl.LINK_STATUS))
        throw new Error(gl.getProgramInfoLog(shaderProgram));

      // Specify to render using that program.
      gl.useProgram(shaderProgram);

      // Get the indexes to communicate vertex attributes to the program.
      var positionAttr = gl.getAttribLocation(shaderProgram, "a_position");
      var colorAttr = gl.getAttribLocation(shaderProgram, "a_color");
      // And specify that we will be actually delivering data to those attributes.
      gl.enableVertexAttribArray(positionAttr);
      gl.enableVertexAttribArray(colorAttr);

      // Store vertex positions and colors in array buffer objects.
      var vertices;
      var positionBuffer = gl.createBuffer();
      gl.bindBuffer(gl.ARRAY_BUFFER, positionBuffer);
      gl.bufferData(gl.ARRAY_BUFFER, new Float32Array(vertices = [
        -0.5, -0.5, 0,
        +0.5, -0.5, 0,
        -0.5, +0.5, 0
      ]), gl.STATIC_DRAW);
      var colorBuffer = gl.createBuffer();
      gl.bindBuffer(gl.ARRAY_BUFFER, colorBuffer);
      gl.bufferData(gl.ARRAY_BUFFER, new Float32Array([
        1, 0, 0, 1,
        0, 1, 0, 1,
        0, 0, 1, 1
      ]), gl.STATIC_DRAW);
      var numVertices = vertices.length / 3; // 3 coordinates per vertex

      // Set GL state
      gl.clearColor(0.3, 0.3, 0.3, 1.0);
      gl.enable(gl.DEPTH_TEST);
      gl.viewport(0, 0, gl.drawingBufferWidth || canvas.width,
                        gl.drawingBufferHeight || canvas.height);

      // Draw scene.
      // If this were an animation, everything after this point would go in a main loop.
      //   Clear frame.
      gl.clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);
      //   Specify the array data to render.
      //   3 and 4 are the lengths of the vectors (3 for XYZ, 4 for RGBA).
      gl.bindBuffer(gl.ARRAY_BUFFER, positionBuffer);
      gl.vertexAttribPointer(positionAttr, 3, gl.FLOAT, false, 0, 0);
      gl.bindBuffer(gl.ARRAY_BUFFER, colorBuffer);
      gl.vertexAttribPointer(colorAttr, 4, gl.FLOAT, false, 0, 0);
      //   Draw triangles using the specified arrays.
      gl.drawArrays(gl.TRIANGLES, 0, numVertices);

      // Check for errors.
      var e;
      while (e = gl.getError())
        console.log("GL error", e);
    </script>
  </body>
</html>
