<?xml version="1.0" encoding="UTF-8"?>
<model ref="r:bcfe2964-5744-4773-9086-0090dbda0712(jetbrains.mps.build.sandbox.build1)">
  <persistence version="9" />
  <languages>
    <use id="798100da-4f0a-421a-b991-71f8c50ce5d2" name="jetbrains.mps.build" version="0" />
    <use id="698a8d22-a104-47a0-ba8d-10e3ec237f13" name="jetbrains.mps.build.workflow" version="0" />
    <use id="479c7a8c-02f9-43b5-9139-d910cb22f298" name="jetbrains.mps.core.xml" version="0" />
  </languages>
  <imports>
    <import index="arit" ref="r:0d66e868-9778-4307-b6f9-4795c00f662f(jetbrains.mps.build.workflow.preset.general)" implicit="true" />
  </imports>
  <registry>
    <language id="698a8d22-a104-47a0-ba8d-10e3ec237f13" name="jetbrains.mps.build.workflow">
      <concept id="2769948622284546677" name="jetbrains.mps.build.workflow.structure.BwfSubTask" flags="ng" index="2VaFvH">
        <child id="2769948622284606050" name="statements" index="2VaTZU" />
      </concept>
      <concept id="2769948622284768359" name="jetbrains.mps.build.workflow.structure.BwfAntStatement" flags="ng" index="2Vbh7Z">
        <child id="2769948622284768360" name="element" index="2Vbh7K" />
      </concept>
      <concept id="3961775458390032824" name="jetbrains.mps.build.workflow.structure.BwfTaskPart" flags="ng" index="3bMsLL">
        <reference id="3961775458390032825" name="task" index="3bMsLK" />
        <child id="3961775458390032826" name="subTasks" index="3bMsLN" />
      </concept>
    </language>
    <language id="479c7a8c-02f9-43b5-9139-d910cb22f298" name="jetbrains.mps.core.xml">
      <concept id="6666499814681415858" name="jetbrains.mps.core.xml.structure.XmlElement" flags="ng" index="2pNNFK">
        <property id="6666499814681415862" name="tagName" index="2pNNFO" />
      </concept>
    </language>
    <language id="798100da-4f0a-421a-b991-71f8c50ce5d2" name="jetbrains.mps.build">
      <concept id="5481553824944787378" name="jetbrains.mps.build.structure.BuildSourceProjectRelativePath" flags="ng" index="55IIr" />
      <concept id="3717132724152913083" name="jetbrains.mps.build.structure.BuildSource_JavaLibraryCP" flags="ng" index="25yagZ">
        <child id="3717132724152913085" name="classpath" index="25yagT" />
      </concept>
      <concept id="3717132724153084007" name="jetbrains.mps.build.structure.BuildSource_JavaJars" flags="ng" index="25yw3z">
        <child id="3717132724153084009" name="jars" index="25yw3H" />
      </concept>
      <concept id="3717132724152589376" name="jetbrains.mps.build.structure.BuildSource_JavaDependencyJar" flags="ng" index="25zrj4">
        <property id="8169228734285428589" name="reexport" index="ECt$H" />
        <child id="3717132724152589377" name="jar" index="25zrj5" />
      </concept>
      <concept id="9126048691955220717" name="jetbrains.mps.build.structure.BuildLayout_File" flags="ng" index="28jJK3">
        <child id="9126048691955220774" name="parameters" index="28jJR8" />
        <child id="9126048691955220762" name="path" index="28jJRO" />
      </concept>
      <concept id="2755237150521975431" name="jetbrains.mps.build.structure.BuildVariableMacroInitWithString" flags="ng" index="aVJcg">
        <child id="2755237150521975437" name="value" index="aVJcq" />
      </concept>
      <concept id="7321017245476976379" name="jetbrains.mps.build.structure.BuildRelativePath" flags="ng" index="iG8Mu">
        <child id="7321017245477039051" name="compositePart" index="iGT6I" />
      </concept>
      <concept id="3767587139141066978" name="jetbrains.mps.build.structure.BuildVariableMacro" flags="ng" index="2kB4xC">
        <child id="2755237150521975432" name="initialValue" index="aVJcv" />
      </concept>
      <concept id="2754769020641646247" name="jetbrains.mps.build.structure.BuildSource_JavaDependencyModule" flags="ng" index="nCB5N">
        <property id="7259033139236507306" name="reexport" index="1Sh$E4" />
        <reference id="2754769020641646250" name="module" index="nCB5Y" />
      </concept>
      <concept id="2754769020641429190" name="jetbrains.mps.build.structure.BuildSource_JavaContentFolder" flags="ng" index="nFU4i">
        <property id="2754769020641429191" name="relativePath" index="nFU4j" />
        <property id="5248329904288265467" name="kind" index="3LZaj6" />
      </concept>
      <concept id="4993211115183325728" name="jetbrains.mps.build.structure.BuildProjectDependency" flags="ng" index="2sgV4H">
        <reference id="5617550519002745380" name="script" index="1l3spb" />
      </concept>
      <concept id="4993211115183250894" name="jetbrains.mps.build.structure.BuildSource_JavaDependencyLibrary" flags="ng" index="2sjeV3">
        <property id="5979287180587196968" name="reexport" index="2a2d0E" />
        <reference id="4993211115183250895" name="library" index="2sjeV2" />
      </concept>
      <concept id="6967233722066057020" name="jetbrains.mps.build.structure.BuildLayout_War" flags="ng" index="2ury4r" />
      <concept id="7801138212747054656" name="jetbrains.mps.build.structure.BuildLayout_Filemode" flags="ng" index="yKbIv">
        <property id="7801138212747054661" name="dirmode" index="yKbIq" />
        <property id="7801138212747054660" name="filemode" index="yKbIr" />
      </concept>
      <concept id="927724900262033858" name="jetbrains.mps.build.structure.BuildSource_JavaOptions" flags="ng" index="2_Ic$z">
        <property id="927724900262398947" name="heapSize" index="2_GNG2" />
        <property id="927724900262398958" name="noWarnings" index="2_GNGf" />
        <property id="927724900262033861" name="generateDebugInfo" index="2_Ic$$" />
        <property id="927724900262033862" name="copyResources" index="2_Ic$B" />
        <property id="6998860900671147996" name="javaLevel" index="TZNOO" />
      </concept>
      <concept id="1500819558096177282" name="jetbrains.mps.build.structure.BuildSource_JavaFiles" flags="ng" index="2GAZfH">
        <child id="1500819558096177283" name="resset" index="2GAZfG" />
      </concept>
      <concept id="2750015747481074431" name="jetbrains.mps.build.structure.BuildLayout_Files" flags="ng" index="2HvfSZ">
        <child id="2750015747481074432" name="path" index="2HvfZ0" />
        <child id="2750015747481074433" name="parameters" index="2HvfZ1" />
      </concept>
      <concept id="1258644073388922138" name="jetbrains.mps.build.structure.BuildSource_JavaJar" flags="ng" index="2HycW7">
        <child id="3717132724152837090" name="path" index="25ysHA" />
      </concept>
      <concept id="4380385936562003279" name="jetbrains.mps.build.structure.BuildString" flags="ng" index="NbPM2">
        <child id="4903714810883783243" name="parts" index="3MwsjC" />
      </concept>
      <concept id="6057319140845467763" name="jetbrains.mps.build.structure.BuildSource_JavaLibrary" flags="ng" index="PiPfp">
        <child id="6057319140845478673" name="elements" index="PiKyV" />
      </concept>
      <concept id="8618885170173601777" name="jetbrains.mps.build.structure.BuildCompositePath" flags="nn" index="2Ry0Ak">
        <property id="8618885170173601779" name="head" index="2Ry0Am" />
        <child id="8618885170173601778" name="tail" index="2Ry0An" />
      </concept>
      <concept id="2591537044435828004" name="jetbrains.mps.build.structure.BuildLayout_CompileOutputOf" flags="ng" index="Saw0i">
        <reference id="2591537044435828006" name="module" index="Saw0g" />
      </concept>
      <concept id="2303926226081001727" name="jetbrains.mps.build.structure.BuildInputSingleFolder" flags="ng" index="TIC1d">
        <child id="2303926226081001728" name="path" index="TIC6M" />
      </concept>
      <concept id="6647099934206700647" name="jetbrains.mps.build.structure.BuildJavaPlugin" flags="ng" index="10PD9b" />
      <concept id="7389400916848050074" name="jetbrains.mps.build.structure.BuildLayout_Jar" flags="ng" index="3981dx" />
      <concept id="7389400916848050071" name="jetbrains.mps.build.structure.BuildLayout_Zip" flags="ng" index="3981dG" />
      <concept id="7389400916848050060" name="jetbrains.mps.build.structure.BuildLayout_NamedContainer" flags="ng" index="3981dR">
        <child id="4380385936562148502" name="containerName" index="Nbhlr" />
      </concept>
      <concept id="7389400916848036984" name="jetbrains.mps.build.structure.BuildLayout_Folder" flags="ng" index="398223" />
      <concept id="7389400916848073810" name="jetbrains.mps.build.structure.BuildSource_JavaContentRoot" flags="ng" index="398b2D">
        <child id="2754769020641429197" name="folders" index="nFU4p" />
        <child id="7389400916848073811" name="basePath" index="398b2C" />
      </concept>
      <concept id="7389400916848073784" name="jetbrains.mps.build.structure.BuildSource_JavaModule" flags="ng" index="398b33">
        <child id="2754769020641646251" name="dependencies" index="nCB5Z" />
        <child id="7389400916848073826" name="sources" index="398b2p" />
      </concept>
      <concept id="7389400916848136194" name="jetbrains.mps.build.structure.BuildFolderMacro" flags="ng" index="398rNT">
        <child id="7389400916848144618" name="defaultPath" index="398pKh" />
      </concept>
      <concept id="7389400916848153117" name="jetbrains.mps.build.structure.BuildSourceMacroRelativePath" flags="ng" index="398BVA">
        <reference id="7389400916848153130" name="macro" index="398BVh" />
      </concept>
      <concept id="4198392933254416812" name="jetbrains.mps.build.structure.BuildLayout_CopyFilterFixCRLF" flags="ng" index="3co7Ac">
        <property id="4198392933254416822" name="eol" index="3co7Am" />
        <property id="4198392933254551900" name="removeEOF" index="3cpA_W" />
      </concept>
      <concept id="2913098736709465755" name="jetbrains.mps.build.structure.BuildLayout_ExportAsJavaLibrary" flags="ng" index="3dmp56">
        <reference id="2913098736709465758" name="library" index="3dmp53" />
      </concept>
      <concept id="3542413272732529456" name="jetbrains.mps.build.structure.BuildNamedLayout" flags="ng" index="1hWdOE" />
      <concept id="5617550519002745364" name="jetbrains.mps.build.structure.BuildLayout" flags="ng" index="1l3spV" />
      <concept id="5617550519002745363" name="jetbrains.mps.build.structure.BuildProject" flags="ng" index="1l3spW">
        <property id="4915877860348071612" name="fileName" index="turDy" />
        <property id="5204048710541015587" name="internalBaseDirectory" index="2DA0ip" />
        <child id="4796668409958418110" name="scriptsDir" index="auvoZ" />
        <child id="6647099934206700656" name="plugins" index="10PD9s" />
        <child id="7389400916848080626" name="parts" index="3989C9" />
        <child id="3542413272732620719" name="aspects" index="1hWBAP" />
        <child id="5617550519002745381" name="dependencies" index="1l3spa" />
        <child id="5617550519002745378" name="macros" index="1l3spd" />
        <child id="5617550519002745372" name="layout" index="1l3spN" />
      </concept>
      <concept id="342830306171203038" name="jetbrains.mps.build.structure.BuildSource_JavaDependencyExternalJarInFolder" flags="ng" index="3tkPu6">
        <property id="342830306171239596" name="suffix" index="3tkGrO" />
        <property id="342830306171234560" name="reexport" index="3tkHdo" />
        <child id="342830306171234561" name="extFolder" index="3tkHdp" />
      </concept>
      <concept id="4701820937132281259" name="jetbrains.mps.build.structure.BuildCustomWorkflow" flags="ng" index="1y0Vig">
        <child id="4701820937132281260" name="parts" index="1y0Vin" />
      </concept>
      <concept id="4701820937132344003" name="jetbrains.mps.build.structure.BuildLayout_Container" flags="ng" index="1y1bJS">
        <child id="7389400916848037006" name="children" index="39821P" />
      </concept>
      <concept id="5610619299013057363" name="jetbrains.mps.build.structure.BuildLayout_ImportContent" flags="ng" index="3ygNvl">
        <reference id="5610619299013057365" name="target" index="3ygNvj" />
      </concept>
      <concept id="5610619299014531647" name="jetbrains.mps.build.structure.BuildSource_JavaExternalJarFolderRef" flags="ng" index="3yqFqT">
        <reference id="5610619299014531648" name="folder" index="3yqFr6" />
      </concept>
      <concept id="841011766565753074" name="jetbrains.mps.build.structure.BuildLayout_Import" flags="ng" index="3_I8Xc">
        <reference id="841011766565753076" name="target" index="3_I8Xa" />
      </concept>
      <concept id="841011766566059607" name="jetbrains.mps.build.structure.BuildStringNotEmpty" flags="ng" index="3_J27D" />
      <concept id="5248329904288051100" name="jetbrains.mps.build.structure.BuildFileIncludeSelector" flags="ng" index="3LWZYx">
        <property id="5248329904288051101" name="pattern" index="3LWZYw" />
      </concept>
      <concept id="5248329904287794596" name="jetbrains.mps.build.structure.BuildInputFiles" flags="ng" index="3LXTmp">
        <child id="5248329904287794598" name="dir" index="3LXTmr" />
        <child id="5248329904287794679" name="selectors" index="3LXTna" />
      </concept>
      <concept id="4903714810883702019" name="jetbrains.mps.build.structure.BuildTextStringPart" flags="ng" index="3Mxwew">
        <property id="4903714810883755350" name="text" index="3MwjfP" />
      </concept>
      <concept id="4903714810883702017" name="jetbrains.mps.build.structure.BuildVarRefStringPart" flags="ng" index="3Mxwey">
        <reference id="4903714810883702018" name="macro" index="3Mxwex" />
      </concept>
    </language>
    <language id="ceab5195-25ea-4f22-9b92-103b95ca8c0c" name="jetbrains.mps.lang.core">
      <concept id="1169194658468" name="jetbrains.mps.lang.core.structure.INamedConcept" flags="ng" index="TrEIO">
        <property id="1169194664001" name="name" index="TrG5h" />
      </concept>
    </language>
  </registry>
  <node concept="1l3spW" id="3_glsEmoD20">
    <property role="2DA0ip" value="samples" />
    <property role="TrG5h" value="buildA" />
    <property role="turDy" value="buildA.xml" />
    <node concept="2_Ic$z" id="6Za9XhmfngA" role="3989C9">
      <property role="2_Ic$$" value="true" />
      <property role="2_GNGf" value="true" />
      <property role="2_GNG2" value="512" />
      <property role="2_Ic$B" value="true" />
      <property role="TZNOO" value="" />
    </node>
    <node concept="PiPfp" id="2xHpXR_dnC3" role="3989C9">
      <property role="TrG5h" value="abcde-lib" />
      <node concept="25yagZ" id="3elU8iQ7vaP" role="PiKyV">
        <node concept="2HycW7" id="3elU8iQ7vaQ" role="25yagT">
          <node concept="55IIr" id="3elU8iQ7vaR" role="25ysHA">
            <node concept="2Ry0Ak" id="3elU8iQ7vaS" role="iGT6I">
              <property role="2Ry0Am" value="buildA" />
              <node concept="2Ry0Ak" id="3elU8iQ7vaT" role="2Ry0An">
                <property role="2Ry0Am" value="abcde.jar" />
              </node>
            </node>
          </node>
        </node>
      </node>
    </node>
    <node concept="398b33" id="60cTC8EAycP" role="3989C9">
      <property role="TrG5h" value="module-A" />
      <node concept="398b2D" id="60cTC8EAycU" role="398b2p">
        <node concept="55IIr" id="60cTC8EAycV" role="398b2C">
          <node concept="2Ry0Ak" id="60cTC8EAycW" role="iGT6I">
            <property role="2Ry0Am" value="buildA" />
          </node>
        </node>
        <node concept="nFU4i" id="60cTC8EAycY" role="nFU4p">
          <property role="3LZaj6" value="4zlO3QTanjS/source" />
          <property role="nFU4j" value="src" />
        </node>
      </node>
      <node concept="2sjeV3" id="2xHpXR_dnTv" role="nCB5Z">
        <property role="2a2d0E" value="true" />
        <ref role="2sjeV2" node="2xHpXR_dnC3" resolve="abcde-lib" />
      </node>
    </node>
    <node concept="398b33" id="2eDSGe9diSc" role="3989C9">
      <property role="TrG5h" value="modX" />
      <node concept="2GAZfH" id="75uV$1rZ1ys" role="398b2p">
        <node concept="TIC1d" id="75uV$1rZ1yt" role="2GAZfG">
          <node concept="55IIr" id="75uV$1rZ1yu" role="TIC6M">
            <node concept="2Ry0Ak" id="75uV$1rZ1yv" role="iGT6I">
              <property role="2Ry0Am" value="buildA" />
              <node concept="2Ry0Ak" id="75uV$1rZ1yw" role="2Ry0An">
                <property role="2Ry0Am" value="src" />
              </node>
            </node>
          </node>
        </node>
      </node>
      <node concept="25zrj4" id="75uV$1s0Luq" role="nCB5Z">
        <property role="ECt$H" value="true" />
        <node concept="2HycW7" id="75uV$1s0Lur" role="25zrj5">
          <node concept="55IIr" id="75uV$1s0Lus" role="25ysHA">
            <node concept="2Ry0Ak" id="75uV$1s0Lut" role="iGT6I">
              <property role="2Ry0Am" value="buildA" />
              <node concept="2Ry0Ak" id="75uV$1s0Luu" role="2Ry0An">
                <property role="2Ry0Am" value="abcde.jar" />
              </node>
            </node>
          </node>
        </node>
      </node>
    </node>
    <node concept="2kB4xC" id="5DY7s5F3VzI" role="1l3spd">
      <property role="TrG5h" value="aver" />
      <node concept="aVJcg" id="6hnvgFycl6e" role="aVJcv">
        <node concept="NbPM2" id="6hnvgFycl6f" role="aVJcq">
          <node concept="3Mxwew" id="6hnvgFycl6g" role="3MwsjC">
            <property role="3MwjfP" value="12.10" />
          </node>
        </node>
      </node>
    </node>
    <node concept="55IIr" id="3_glsEmoD21" role="auvoZ">
      <node concept="2Ry0Ak" id="3_glsEmoD22" role="iGT6I">
        <property role="2Ry0Am" value="buildA" />
      </node>
    </node>
    <node concept="1l3spV" id="3_glsEmoD23" role="1l3spN">
      <node concept="398223" id="5DY7s5F3VzN" role="39821P">
        <node concept="3_J27D" id="5DY7s5F3VzO" role="Nbhlr">
          <node concept="3Mxwew" id="5DY7s5F3VzP" role="3MwsjC">
            <property role="3MwjfP" value="result" />
          </node>
        </node>
        <node concept="3981dG" id="5DY7s5F3VzT" role="39821P">
          <node concept="3_J27D" id="5DY7s5F3VzU" role="Nbhlr">
            <node concept="3Mxwew" id="5DY7s5F3VzW" role="3MwsjC">
              <property role="3MwjfP" value="A-" />
            </node>
            <node concept="3Mxwey" id="5DY7s5F3VzX" role="3MwsjC">
              <ref role="3Mxwex" node="5DY7s5F3VzI" resolve="aver" />
            </node>
            <node concept="3Mxwew" id="5DY7s5F3VzY" role="3MwsjC">
              <property role="3MwjfP" value=".zip" />
            </node>
          </node>
          <node concept="3981dx" id="60cTC8EBbEi" role="39821P">
            <node concept="3_J27D" id="60cTC8EBbEj" role="Nbhlr">
              <node concept="3Mxwew" id="60cTC8EBbEk" role="3MwsjC">
                <property role="3MwjfP" value="module-A.jar" />
              </node>
            </node>
            <node concept="Saw0i" id="2fQZjorSjPV" role="39821P">
              <ref role="Saw0g" node="60cTC8EAycP" resolve="module-A" />
            </node>
          </node>
          <node concept="398223" id="5DY7s5F3V$2" role="39821P">
            <node concept="3_J27D" id="5DY7s5F3V$3" role="Nbhlr">
              <node concept="3Mxwew" id="5DY7s5F3V$c" role="3MwsjC">
                <property role="3MwjfP" value="A" />
              </node>
            </node>
            <node concept="28jJK3" id="35zoHQHSGd1" role="39821P">
              <node concept="55IIr" id="5DY7s5F3V$7" role="28jJRO">
                <node concept="2Ry0Ak" id="5DY7s5F3V$8" role="iGT6I">
                  <property role="2Ry0Am" value="testdata" />
                  <node concept="2Ry0Ak" id="5DY7s5F3V$9" role="2Ry0An">
                    <property role="2Ry0Am" value="a.txt" />
                  </node>
                </node>
              </node>
            </node>
          </node>
          <node concept="3981dG" id="2eDSGe9dj3y" role="39821P">
            <node concept="3_J27D" id="2eDSGe9dj3z" role="Nbhlr">
              <node concept="3Mxwew" id="2eDSGe9dj3$" role="3MwsjC">
                <property role="3MwjfP" value="withX.zip" />
              </node>
            </node>
            <node concept="3981dx" id="2eDSGe9dj3A" role="39821P">
              <node concept="3_J27D" id="2eDSGe9dj3B" role="Nbhlr">
                <node concept="3Mxwew" id="2eDSGe9dj3C" role="3MwsjC">
                  <property role="3MwjfP" value="X.jar" />
                </node>
              </node>
              <node concept="Saw0i" id="2eDSGe9dj3D" role="39821P">
                <ref role="Saw0g" node="2eDSGe9diSc" resolve="modX" />
              </node>
            </node>
          </node>
          <node concept="3981dG" id="75uV$1rZ2lZ" role="39821P">
            <node concept="3dmp56" id="6wK_PpNBTMF" role="39821P">
              <ref role="3dmp53" node="2xHpXR_dnC3" resolve="abcde-lib" />
              <node concept="28jJK3" id="35zoHQHSGcZ" role="39821P">
                <node concept="55IIr" id="6wK_PpNBTMI" role="28jJRO">
                  <node concept="2Ry0Ak" id="6wK_PpNBTMK" role="iGT6I">
                    <property role="2Ry0Am" value="buildA" />
                    <node concept="2Ry0Ak" id="6wK_PpNBTML" role="2Ry0An">
                      <property role="2Ry0Am" value="abcde.jar" />
                    </node>
                  </node>
                </node>
              </node>
            </node>
            <node concept="3_J27D" id="75uV$1rZ2m0" role="Nbhlr">
              <node concept="3Mxwew" id="75uV$1rZ2m1" role="3MwsjC">
                <property role="3MwjfP" value="abcde.zip" />
              </node>
            </node>
          </node>
        </node>
      </node>
    </node>
    <node concept="10PD9b" id="60cTC8EAycR" role="10PD9s" />
  </node>
  <node concept="1l3spW" id="3_glsEmoD24">
    <property role="2DA0ip" value="samples/subprojects/buildB" />
    <property role="TrG5h" value="buildB" />
    <property role="turDy" value="buildB.xml" />
    <node concept="1l3spV" id="3_glsEmoD27" role="1l3spN">
      <node concept="398223" id="5DY7s5F3V$f" role="39821P">
        <node concept="3_J27D" id="5DY7s5F3V$g" role="Nbhlr">
          <node concept="3Mxwew" id="5DY7s5F3V$h" role="3MwsjC">
            <property role="3MwjfP" value="result" />
          </node>
        </node>
        <node concept="28jJK3" id="35zoHQHSGdb" role="39821P">
          <node concept="55IIr" id="3D3G23Q9t_l" role="28jJRO">
            <node concept="2Ry0Ak" id="3D3G23Q9Jxu" role="iGT6I">
              <property role="2Ry0Am" value="buildB.xml" />
            </node>
          </node>
          <node concept="3co7Ac" id="3D3G23Q9t_m" role="28jJR8">
            <property role="3co7Am" value="3D3G23Q8WAM/crlf" />
            <property role="3cpA_W" value="true" />
          </node>
        </node>
        <node concept="28jJK3" id="35zoHQHSGd9" role="39821P">
          <node concept="55IIr" id="3D3G23Q9Jx_" role="28jJRO">
            <node concept="2Ry0Ak" id="3D3G23Q9JxB" role="iGT6I">
              <property role="2Ry0Am" value=".." />
              <node concept="2Ry0Ak" id="3D3G23Q9JxC" role="2Ry0An">
                <property role="2Ry0Am" value=".." />
                <node concept="2Ry0Ak" id="3D3G23Q9JxD" role="2Ry0An">
                  <property role="2Ry0Am" value="build.xml" />
                </node>
              </node>
            </node>
          </node>
          <node concept="3co7Ac" id="3D3G23Q9Jxw" role="28jJR8">
            <property role="3co7Am" value="3D3G23Q8WAK/cr" />
          </node>
        </node>
        <node concept="398223" id="5DY7s5F3V$m" role="39821P">
          <node concept="3_J27D" id="5DY7s5F3V$n" role="Nbhlr">
            <node concept="3Mxwew" id="5DY7s5F3V$o" role="3MwsjC">
              <property role="3MwjfP" value="A-" />
            </node>
            <node concept="3Mxwey" id="5DY7s5F3V$p" role="3MwsjC">
              <ref role="3Mxwex" node="5DY7s5F3VzI" resolve="aver" />
            </node>
          </node>
          <node concept="3_I8Xc" id="5DY7s5F3V$j" role="39821P">
            <ref role="3_I8Xa" node="5DY7s5F3V$2" resolve="A" />
          </node>
          <node concept="3_I8Xc" id="60cTC8EBbVc" role="39821P">
            <ref role="3_I8Xa" node="60cTC8EBbEi" resolve="module-A.jar" />
          </node>
          <node concept="3_I8Xc" id="1bWeed$oW9D" role="39821P">
            <ref role="3_I8Xa" node="35zoHQHSGcZ" />
          </node>
          <node concept="3981dx" id="60cTC8EBbVe" role="39821P">
            <node concept="3_J27D" id="60cTC8EBbVf" role="Nbhlr">
              <node concept="3Mxwew" id="60cTC8EBbVg" role="3MwsjC">
                <property role="3MwjfP" value="module-B.jar" />
              </node>
            </node>
            <node concept="Saw0i" id="2fQZjorSjPW" role="39821P">
              <ref role="Saw0g" node="60cTC8EAycZ" resolve="module-B" />
            </node>
          </node>
        </node>
      </node>
      <node concept="398223" id="5N5c1exvwki" role="39821P">
        <node concept="3_J27D" id="5N5c1exvwkj" role="Nbhlr">
          <node concept="3Mxwew" id="5N5c1exvwkk" role="3MwsjC">
            <property role="3MwjfP" value="importTest" />
          </node>
        </node>
        <node concept="3981dG" id="5N5c1exvwkn" role="39821P">
          <node concept="3_J27D" id="5N5c1exvwko" role="Nbhlr">
            <node concept="3Mxwew" id="5N5c1exvwkp" role="3MwsjC">
              <property role="3MwjfP" value="importResult.zip" />
            </node>
          </node>
          <node concept="yKbIv" id="6L3dtXextPK" role="39821P">
            <property role="yKbIr" value="754" />
            <property role="yKbIq" value="752" />
            <node concept="3_I8Xc" id="5N5c1exvwkq" role="39821P">
              <ref role="3_I8Xa" node="5DY7s5F3VzN" resolve="result" />
            </node>
            <node concept="3ygNvl" id="6L3dtXexQBB" role="39821P">
              <ref role="3ygNvj" node="5DY7s5F3VzT" resolve="A-${aver}.zip" />
            </node>
            <node concept="28jJK3" id="35zoHQHSGcS" role="39821P">
              <node concept="55IIr" id="6L3dtXex_Ts" role="28jJRO">
                <node concept="2Ry0Ak" id="6L3dtXex_Tt" role="iGT6I">
                  <property role="2Ry0Am" value="buildB.xml" />
                </node>
              </node>
            </node>
            <node concept="2HvfSZ" id="35zoHQHSGd4" role="39821P">
              <node concept="55IIr" id="6L3dtXexPca" role="2HvfZ0">
                <node concept="2Ry0Ak" id="6L3dtXexPcb" role="iGT6I">
                  <property role="2Ry0Am" value="moduleB" />
                  <node concept="2Ry0Ak" id="6L3dtXexPcc" role="2Ry0An">
                    <property role="2Ry0Am" value="src" />
                    <node concept="2Ry0Ak" id="6L3dtXexPcd" role="2Ry0An">
                      <property role="2Ry0Am" value="jetbrains" />
                      <node concept="2Ry0Ak" id="6L3dtXexPce" role="2Ry0An">
                        <property role="2Ry0Am" value="moduleB" />
                      </node>
                    </node>
                  </node>
                </node>
              </node>
            </node>
            <node concept="398223" id="6L3dtXexQQv" role="39821P">
              <node concept="3_J27D" id="6L3dtXexQQw" role="Nbhlr">
                <node concept="3Mxwew" id="6L3dtXexQQx" role="3MwsjC">
                  <property role="3MwjfP" value="aaa" />
                </node>
              </node>
              <node concept="28jJK3" id="35zoHQHSGd8" role="39821P">
                <node concept="55IIr" id="6L3dtXexQQ$" role="28jJRO">
                  <node concept="2Ry0Ak" id="6L3dtXexQQ_" role="iGT6I">
                    <property role="2Ry0Am" value="buildB.xml" />
                  </node>
                </node>
                <node concept="3co7Ac" id="6L3dtXexR5Q" role="28jJR8">
                  <property role="3co7Am" value="3D3G23Q8WAM/crlf" />
                </node>
              </node>
            </node>
          </node>
        </node>
        <node concept="3981dG" id="5N5c1exvwkr" role="39821P">
          <node concept="3_J27D" id="5N5c1exvwks" role="Nbhlr">
            <node concept="3Mxwew" id="5N5c1exvwkt" role="3MwsjC">
              <property role="3MwjfP" value="importResultContent.zip" />
            </node>
          </node>
          <node concept="3ygNvl" id="5N5c1exvwkv" role="39821P">
            <ref role="3ygNvj" node="5DY7s5F3VzN" resolve="result" />
          </node>
        </node>
      </node>
    </node>
    <node concept="55IIr" id="3_glsEmoOJi" role="auvoZ" />
    <node concept="2sgV4H" id="5DY7s5F3V$i" role="1l3spa">
      <ref role="1l3spb" node="3_glsEmoD20" resolve="buildA" />
    </node>
    <node concept="398b33" id="60cTC8EAycZ" role="3989C9">
      <property role="TrG5h" value="module-B" />
      <node concept="398b2D" id="60cTC8EAyd0" role="398b2p">
        <node concept="55IIr" id="60cTC8EAyd1" role="398b2C">
          <node concept="2Ry0Ak" id="60cTC8EAyd2" role="iGT6I">
            <property role="2Ry0Am" value="moduleB" />
          </node>
        </node>
        <node concept="nFU4i" id="60cTC8EAyd3" role="nFU4p">
          <property role="3LZaj6" value="4zlO3QTanjS/source" />
          <property role="nFU4j" value="src" />
        </node>
      </node>
      <node concept="nCB5N" id="60cTC8EAyd5" role="nCB5Z">
        <ref role="nCB5Y" node="60cTC8EAycP" resolve="module-A" />
      </node>
    </node>
    <node concept="10PD9b" id="60cTC8EAyd4" role="10PD9s" />
  </node>
  <node concept="1l3spW" id="4lbsKRp2ybs">
    <property role="TrG5h" value="buildPlugin" />
    <property role="2DA0ip" value="samples" />
    <property role="turDy" value="buildPlugin.xml" />
    <node concept="398b33" id="5FtnUVJQFNx" role="3989C9">
      <property role="TrG5h" value="test1" />
      <node concept="nCB5N" id="5FtnUVJQFNy" role="nCB5Z">
        <ref role="nCB5Y" node="3d9yYFirPpA" resolve="mps-core" />
      </node>
    </node>
    <node concept="1l3spV" id="4lbsKRp2ybt" role="1l3spN">
      <node concept="398223" id="34DbxDwS0Bv" role="39821P">
        <property role="TrG5h" value="xx" />
        <node concept="3981dx" id="34DbxDwS0Bw" role="39821P">
          <property role="TrG5h" value="xx" />
          <node concept="398223" id="3_glsEmoicM" role="39821P">
            <node concept="3_J27D" id="IFRVVI697j" role="Nbhlr">
              <node concept="3Mxwew" id="IFRVVI6a_T" role="3MwsjC">
                <property role="3MwjfP" value="test" />
              </node>
            </node>
            <node concept="2ury4r" id="62K_yvYSE27" role="39821P">
              <node concept="3_J27D" id="62K_yvYSE28" role="Nbhlr">
                <node concept="3Mxwew" id="62K_yvYSE2a" role="3MwsjC">
                  <property role="3MwjfP" value="aaa" />
                </node>
                <node concept="3Mxwey" id="62K_yvYSE2c" role="3MwsjC">
                  <ref role="3Mxwex" node="79gE8jhMEP4" resolve="ver" />
                </node>
                <node concept="3Mxwew" id="62K_yvYSE2b" role="3MwsjC">
                  <property role="3MwjfP" value=".war" />
                </node>
              </node>
              <node concept="Saw0i" id="62K_yvYSE29" role="39821P">
                <ref role="Saw0g" node="5FtnUVJQFNx" resolve="test1" />
              </node>
            </node>
          </node>
          <node concept="3_J27D" id="IFRVVI697h" role="Nbhlr">
            <node concept="3Mxwew" id="IFRVVI697i" role="3MwsjC">
              <property role="3MwjfP" value="xx" />
            </node>
          </node>
        </node>
        <node concept="3_J27D" id="IFRVVI697f" role="Nbhlr">
          <node concept="3Mxwew" id="IFRVVI697g" role="3MwsjC">
            <property role="3MwjfP" value="xx" />
          </node>
        </node>
      </node>
    </node>
    <node concept="2sgV4H" id="4lbsKRp2zhR" role="1l3spa">
      <ref role="1l3spb" node="4lbsKRp2riZ" resolve="buildCore" />
    </node>
    <node concept="2sgV4H" id="3_glsEmoD29" role="1l3spa">
      <ref role="1l3spb" node="3_glsEmoD20" resolve="buildA" />
    </node>
    <node concept="2sgV4H" id="3_glsEmoD2c" role="1l3spa">
      <ref role="1l3spb" node="3_glsEmoD24" resolve="buildB" />
    </node>
    <node concept="55IIr" id="4vrYmjR02Wq" role="auvoZ">
      <node concept="2Ry0Ak" id="4vrYmjR02Wr" role="iGT6I">
        <property role="2Ry0Am" value="buildPlugin" />
      </node>
    </node>
    <node concept="1hWdOE" id="3_glsEmnJq5" role="1hWBAP">
      <property role="TrG5h" value="custom1" />
      <node concept="3981dG" id="450ejGzh7TE" role="39821P">
        <node concept="3_J27D" id="450ejGzh7TF" role="Nbhlr">
          <node concept="3Mxwew" id="450ejGzh7TG" role="3MwsjC">
            <property role="3MwjfP" value="qas.zip" />
          </node>
        </node>
        <node concept="398223" id="450ejGzh7TH" role="39821P">
          <node concept="3_J27D" id="450ejGzh7TI" role="Nbhlr">
            <node concept="3Mxwew" id="450ejGzh7TJ" role="3MwsjC">
              <property role="3MwjfP" value="a" />
            </node>
          </node>
          <node concept="3981dx" id="62K_yvYSE2e" role="39821P">
            <node concept="3_J27D" id="62K_yvYSE2g" role="Nbhlr">
              <node concept="3Mxwew" id="62K_yvYSE2h" role="3MwsjC">
                <property role="3MwjfP" value="test1" />
              </node>
              <node concept="3Mxwey" id="62K_yvYSE2i" role="3MwsjC">
                <ref role="3Mxwex" node="79gE8jhMEP4" resolve="ver" />
              </node>
              <node concept="3Mxwew" id="62K_yvYSE2j" role="3MwsjC">
                <property role="3MwjfP" value=".jar" />
              </node>
            </node>
          </node>
        </node>
      </node>
    </node>
    <node concept="1y0Vig" id="450ejGzh6pW" role="1hWBAP">
      <node concept="3bMsLL" id="450ejGzh6pX" role="1y0Vin">
        <ref role="3bMsLK" to="arit:450ejGzgRPq" resolve="assemble" />
        <node concept="2VaFvH" id="450ejGzh6q7" role="3bMsLN">
          <property role="TrG5h" value="aaa" />
          <node concept="2Vbh7Z" id="450ejGzh6q8" role="2VaTZU">
            <node concept="2pNNFK" id="450ejGzh6qa" role="2Vbh7K">
              <property role="2pNNFO" value="aaa" />
            </node>
          </node>
        </node>
      </node>
    </node>
    <node concept="398rNT" id="3_glsEmowdQ" role="1l3spd">
      <property role="TrG5h" value="ds" />
      <node concept="55IIr" id="3_glsEmowdR" role="398pKh">
        <node concept="2Ry0Ak" id="3_glsEmowdS" role="iGT6I">
          <property role="2Ry0Am" value="" />
        </node>
      </node>
    </node>
    <node concept="10PD9b" id="5FtnUVJQFNz" role="10PD9s" />
  </node>
  <node concept="1l3spW" id="4lbsKRp2riZ">
    <property role="TrG5h" value="buildCore" />
    <property role="2DA0ip" value="samples" />
    <property role="turDy" value="buildCore.xml" />
    <node concept="10PD9b" id="5KZfyKsVnwe" role="10PD9s" />
    <node concept="PiPfp" id="3d9yYFirPpv" role="3989C9">
      <property role="TrG5h" value="apache-collections" />
      <node concept="25yagZ" id="3elU8iQ7vaV" role="PiKyV">
        <node concept="2HycW7" id="3elU8iQ7vaW" role="25yagT">
          <node concept="398BVA" id="3elU8iQ7vaY" role="25ysHA">
            <ref role="398BVh" node="Y2EImGIfYI" resolve="build_langs" />
            <node concept="2Ry0Ak" id="3elU8iQ7vaZ" role="iGT6I">
              <property role="2Ry0Am" value="build.jar" />
            </node>
          </node>
        </node>
      </node>
      <node concept="25yagZ" id="3elU8iQ8jMi" role="PiKyV">
        <node concept="25yw3z" id="3elU8iQ8jMj" role="25yagT">
          <node concept="3LXTmp" id="3elU8iQ8jMk" role="25yw3H">
            <node concept="398BVA" id="3elU8iQ8jMm" role="3LXTmr">
              <ref role="398BVh" node="4lbsKRp2spQ" resolve="mps_home" />
            </node>
            <node concept="3LWZYx" id="3elU8iQ8jXZ" role="3LXTna">
              <property role="3LWZYw" value="**/*.jar" />
            </node>
          </node>
        </node>
      </node>
    </node>
    <node concept="398b33" id="3d9yYFirPpA" role="3989C9">
      <property role="TrG5h" value="mps-core" />
      <node concept="398b2D" id="4zlO3QTaxaT" role="398b2p">
        <node concept="398BVA" id="4zlO3QTaxaV" role="398b2C">
          <ref role="398BVh" node="4lbsKRp2spQ" resolve="mps_home" />
          <node concept="2Ry0Ak" id="4zlO3QTaxfI" role="iGT6I">
            <property role="2Ry0Am" value="core" />
          </node>
        </node>
        <node concept="nFU4i" id="4zlO3QTaxaW" role="nFU4p">
          <property role="nFU4j" value="source" />
          <property role="3LZaj6" value="4zlO3QTanjS/source" />
        </node>
        <node concept="nFU4i" id="6S1jmf0wEKQ" role="nFU4p">
          <property role="3LZaj6" value="4zlO3QTanjS/source" />
          <property role="nFU4j" value="source_gen" />
        </node>
        <node concept="nFU4i" id="4zlO3QTa$mb" role="nFU4p">
          <property role="3LZaj6" value="4zlO3QTanjT/test" />
          <property role="nFU4j" value="tests" />
        </node>
      </node>
      <node concept="2sjeV3" id="5JSLLANKPs4" role="nCB5Z">
        <ref role="2sjeV2" node="3d9yYFirPpv" resolve="apache-collections" />
      </node>
    </node>
    <node concept="1l3spV" id="4lbsKRp2rj0" role="1l3spN">
      <node concept="398223" id="4lbsKRp2rj3" role="39821P">
        <property role="TrG5h" value="default" />
        <node concept="3981dG" id="Y2EImGIn8C" role="39821P">
          <property role="TrG5h" value="release.zip" />
          <node concept="3981dG" id="Y2EImGIn8H" role="39821P">
            <property role="TrG5h" value="inrelease.zip" />
            <node concept="3981dx" id="IFRVVI6970" role="39821P">
              <node concept="Saw0i" id="2fQZjorSjPX" role="39821P">
                <ref role="Saw0g" node="3d9yYFirPpA" resolve="mps-core" />
              </node>
              <node concept="398223" id="Y2EImGIn8z" role="39821P">
                <property role="TrG5h" value="abc" />
                <node concept="398223" id="Y2EImGIn8$" role="39821P">
                  <property role="TrG5h" value="edf" />
                  <node concept="3981dG" id="Y2EImGIn8_" role="39821P">
                    <property role="TrG5h" value="aaa" />
                    <node concept="3_J27D" id="IFRVVI697a" role="Nbhlr">
                      <node concept="3Mxwew" id="IFRVVI697b" role="3MwsjC">
                        <property role="3MwjfP" value="aaa" />
                      </node>
                    </node>
                  </node>
                  <node concept="3_J27D" id="IFRVVI6978" role="Nbhlr">
                    <node concept="3Mxwew" id="IFRVVI6979" role="3MwsjC">
                      <property role="3MwjfP" value="edf" />
                    </node>
                  </node>
                </node>
                <node concept="3_J27D" id="IFRVVI6976" role="Nbhlr">
                  <node concept="3Mxwew" id="IFRVVI6977" role="3MwsjC">
                    <property role="3MwjfP" value="abc" />
                  </node>
                </node>
              </node>
              <node concept="3_J27D" id="IFRVVI6971" role="Nbhlr">
                <node concept="3Mxwew" id="IFRVVI6973" role="3MwsjC">
                  <property role="3MwjfP" value="mps-core" />
                </node>
                <node concept="3Mxwey" id="IFRVVI6974" role="3MwsjC">
                  <ref role="3Mxwex" node="79gE8jhMEP4" resolve="ver" />
                </node>
                <node concept="3Mxwew" id="IFRVVI6975" role="3MwsjC">
                  <property role="3MwjfP" value=".jar" />
                </node>
              </node>
            </node>
            <node concept="28jJK3" id="35zoHQHSGcU" role="39821P">
              <node concept="398BVA" id="4zlO3QT9qgt" role="28jJRO">
                <ref role="398BVh" node="4lbsKRp2spQ" resolve="mps_home" />
                <node concept="2Ry0Ak" id="4zlO3QT9CXB" role="iGT6I">
                  <property role="2Ry0Am" value="MPS.ipr" />
                </node>
              </node>
            </node>
            <node concept="2HvfSZ" id="35zoHQHSGd5" role="39821P">
              <node concept="55IIr" id="4zlO3QT9w4f" role="2HvfZ0">
                <node concept="2Ry0Ak" id="4zlO3QT9w4g" role="iGT6I">
                  <property role="2Ry0Am" value="source_gen" />
                  <node concept="2Ry0Ak" id="4zlO3QT9w4j" role="2Ry0An">
                    <property role="2Ry0Am" value="" />
                  </node>
                </node>
              </node>
              <node concept="3LWZYx" id="4zlO3QT9CXo" role="2HvfZ1">
                <property role="3LWZYw" value="**/*.java" />
              </node>
            </node>
            <node concept="3_J27D" id="IFRVVI696T" role="Nbhlr">
              <node concept="3Mxwew" id="IFRVVI696U" role="3MwsjC">
                <property role="3MwjfP" value="inrelease.zip" />
              </node>
            </node>
          </node>
          <node concept="398223" id="Y2EImGIn8E" role="39821P">
            <property role="TrG5h" value="aaa" />
            <node concept="398223" id="Y2EImGIn8F" role="39821P">
              <property role="TrG5h" value="bbb" />
              <node concept="28jJK3" id="35zoHQHSGd2" role="39821P">
                <node concept="398BVA" id="4zlO3QTa1cP" role="28jJRO">
                  <ref role="398BVh" node="4lbsKRp2spQ" resolve="mps_home" />
                  <node concept="2Ry0Ak" id="4zlO3QTa1cQ" role="iGT6I">
                    <property role="2Ry0Am" value="about.txt" />
                  </node>
                </node>
              </node>
              <node concept="3_J27D" id="IFRVVI697d" role="Nbhlr">
                <node concept="3Mxwew" id="IFRVVI697e" role="3MwsjC">
                  <property role="3MwjfP" value="bbb" />
                </node>
              </node>
            </node>
            <node concept="3_J27D" id="IFRVVI696V" role="Nbhlr">
              <node concept="3Mxwew" id="IFRVVI696W" role="3MwsjC">
                <property role="3MwjfP" value="aaa" />
              </node>
            </node>
          </node>
          <node concept="3_J27D" id="IFRVVI696R" role="Nbhlr">
            <node concept="3Mxwew" id="IFRVVI696S" role="3MwsjC">
              <property role="3MwjfP" value="release.zip" />
            </node>
          </node>
        </node>
        <node concept="3_J27D" id="IFRVVI696P" role="Nbhlr">
          <node concept="3Mxwew" id="IFRVVI696Q" role="3MwsjC">
            <property role="3MwjfP" value="default" />
          </node>
        </node>
      </node>
    </node>
    <node concept="398rNT" id="4lbsKRp2spQ" role="1l3spd">
      <property role="TrG5h" value="mps_home" />
      <node concept="55IIr" id="4jjtc7X07JX" role="398pKh">
        <node concept="2Ry0Ak" id="4jjtc7X0ati" role="iGT6I">
          <property role="2Ry0Am" value=".." />
          <node concept="2Ry0Ak" id="4jjtc7X0atk" role="2Ry0An">
            <property role="2Ry0Am" value=".." />
            <node concept="2Ry0Ak" id="4jjtc7X0atl" role="2Ry0An">
              <property role="2Ry0Am" value=".." />
              <node concept="2Ry0Ak" id="4jjtc7X0atm" role="2Ry0An">
                <property role="2Ry0Am" value=".." />
              </node>
            </node>
          </node>
        </node>
      </node>
    </node>
    <node concept="398rNT" id="Y2EImGIfYI" role="1l3spd">
      <property role="TrG5h" value="build_langs" />
      <node concept="398BVA" id="Y2EImGIfYJ" role="398pKh">
        <ref role="398BVh" node="4lbsKRp2spQ" resolve="mps_home" />
        <node concept="2Ry0Ak" id="Y2EImGIfYK" role="iGT6I">
          <property role="2Ry0Am" value="languages" />
          <node concept="2Ry0Ak" id="Y2EImGIfYL" role="2Ry0An">
            <property role="2Ry0Am" value="build" />
          </node>
        </node>
      </node>
    </node>
    <node concept="2kB4xC" id="4gdvEeQzNxf" role="1l3spd">
      <property role="TrG5h" value="build.number" />
    </node>
    <node concept="2kB4xC" id="79gE8jhMEP4" role="1l3spd">
      <property role="TrG5h" value="ver" />
    </node>
    <node concept="2kB4xC" id="5$qwiKF6jaX" role="1l3spd">
      <property role="TrG5h" value="MPS" />
      <node concept="aVJcg" id="6hnvgFycl6i" role="aVJcv">
        <node concept="NbPM2" id="6hnvgFycl6j" role="aVJcq">
          <node concept="3Mxwew" id="6hnvgFycl6k" role="3MwsjC">
            <property role="3MwjfP" value="MPS-" />
          </node>
          <node concept="3Mxwey" id="6hnvgFycl6l" role="3MwsjC">
            <ref role="3Mxwex" node="4gdvEeQzNxf" resolve="build.number" />
          </node>
        </node>
      </node>
    </node>
    <node concept="55IIr" id="4vrYmjR02Wo" role="auvoZ">
      <node concept="2Ry0Ak" id="4vrYmjR02Wp" role="iGT6I">
        <property role="2Ry0Am" value="buildCore" />
      </node>
    </node>
  </node>
  <node concept="1l3spW" id="2UMAsGM8_bc">
    <property role="TrG5h" value="buildC" />
    <property role="2DA0ip" value="samples" />
    <property role="turDy" value="buildC.xml" />
    <node concept="55IIr" id="2UMAsGM8_bd" role="auvoZ">
      <node concept="2Ry0Ak" id="2UMAsGM8_be" role="iGT6I">
        <property role="2Ry0Am" value="buildC" />
      </node>
    </node>
    <node concept="1l3spV" id="2UMAsGM8_bf" role="1l3spN">
      <node concept="398223" id="2UMAsGM8_bj" role="39821P">
        <node concept="3_J27D" id="2UMAsGM8_bk" role="Nbhlr">
          <node concept="3Mxwew" id="2UMAsGM8_bl" role="3MwsjC">
            <property role="3MwjfP" value="as" />
          </node>
        </node>
        <node concept="3_I8Xc" id="2UMAsGM8_bm" role="39821P">
          <ref role="3_I8Xa" node="35zoHQHSGd1" />
        </node>
      </node>
    </node>
    <node concept="10PD9b" id="2UMAsGM8_bg" role="10PD9s" />
    <node concept="2sgV4H" id="2UMAsGM8_bh" role="1l3spa">
      <ref role="1l3spb" node="3_glsEmoD24" resolve="buildB" />
    </node>
  </node>
  <node concept="1l3spW" id="2eDSGe9diSi">
    <property role="TrG5h" value="buildD" />
    <property role="2DA0ip" value="samples" />
    <property role="turDy" value="buildD.xml" />
    <node concept="398b33" id="2eDSGe9diSo" role="3989C9">
      <property role="TrG5h" value="AAA" />
      <node concept="nCB5N" id="2eDSGe9diSp" role="nCB5Z">
        <ref role="nCB5Y" node="2eDSGe9diSc" resolve="modX" />
      </node>
      <node concept="2GAZfH" id="75uV$1rZ2Kb" role="398b2p">
        <node concept="TIC1d" id="75uV$1rZ2Kc" role="2GAZfG">
          <node concept="55IIr" id="75uV$1rZ2Kd" role="TIC6M">
            <node concept="2Ry0Ak" id="75uV$1rZ2Ke" role="iGT6I">
              <property role="2Ry0Am" value="buildD" />
              <node concept="2Ry0Ak" id="75uV$1rZ2Kf" role="2Ry0An">
                <property role="2Ry0Am" value="src" />
              </node>
            </node>
          </node>
        </node>
      </node>
    </node>
    <node concept="55IIr" id="2eDSGe9diSj" role="auvoZ">
      <node concept="2Ry0Ak" id="75uV$1rZ15_" role="iGT6I">
        <property role="2Ry0Am" value="buildD" />
      </node>
    </node>
    <node concept="1l3spV" id="2eDSGe9diSk" role="1l3spN">
      <node concept="3981dx" id="2eDSGe9diSq" role="39821P">
        <node concept="3_J27D" id="2eDSGe9diSr" role="Nbhlr">
          <node concept="3Mxwew" id="2eDSGe9diSs" role="3MwsjC">
            <property role="3MwjfP" value="aaaa.jar" />
          </node>
        </node>
        <node concept="Saw0i" id="2eDSGe9diSt" role="39821P">
          <ref role="Saw0g" node="2eDSGe9diSo" resolve="AAA" />
        </node>
      </node>
    </node>
    <node concept="10PD9b" id="2eDSGe9diSl" role="10PD9s" />
    <node concept="2sgV4H" id="2eDSGe9diSm" role="1l3spa">
      <ref role="1l3spb" node="3_glsEmoD20" resolve="buildA" />
    </node>
  </node>
  <node concept="1l3spW" id="4RsV8qJCzKh">
    <property role="2DA0ip" value="samples" />
    <property role="TrG5h" value="buildE" />
    <property role="turDy" value="buildE.xml" />
    <node concept="55IIr" id="4RsV8qJCzKi" role="auvoZ">
      <node concept="2Ry0Ak" id="4RsV8qJCzKw" role="iGT6I">
        <property role="2Ry0Am" value="buildE" />
      </node>
    </node>
    <node concept="1l3spV" id="4RsV8qJCzKj" role="1l3spN">
      <node concept="398223" id="4RsV8qJCzKk" role="39821P">
        <node concept="3_J27D" id="4RsV8qJCzKl" role="Nbhlr">
          <node concept="3Mxwew" id="4RsV8qJCzKo" role="3MwsjC">
            <property role="3MwjfP" value="Content" />
          </node>
        </node>
        <node concept="3ygNvl" id="4RsV8qJCzKp" role="39821P">
          <ref role="3ygNvj" node="5DY7s5F3VzT" resolve="A-${aver}.zip" />
        </node>
      </node>
      <node concept="3981dG" id="4RsV8qJD1hX" role="39821P">
        <node concept="3ygNvl" id="4RsV8qJD1i0" role="39821P">
          <ref role="3ygNvj" node="2eDSGe9dj3y" resolve="withX.zip" />
        </node>
        <node concept="3_J27D" id="4RsV8qJD1hY" role="Nbhlr">
          <node concept="3Mxwew" id="4RsV8qJD1hZ" role="3MwsjC">
            <property role="3MwjfP" value="packedContent.zip" />
          </node>
        </node>
      </node>
      <node concept="3981dG" id="4RsV8qJD1i2" role="39821P">
        <node concept="3ygNvl" id="4RsV8qJD1i5" role="39821P">
          <ref role="3ygNvj" node="5DY7s5F3VzT" resolve="A-${aver}.zip" />
        </node>
        <node concept="3_J27D" id="4RsV8qJD1i3" role="Nbhlr">
          <node concept="3Mxwew" id="4RsV8qJD1i4" role="3MwsjC">
            <property role="3MwjfP" value="packedContent2.zip" />
          </node>
        </node>
      </node>
      <node concept="3981dx" id="4RsV8qJDdZ2" role="39821P">
        <node concept="3_J27D" id="4RsV8qJDdZ3" role="Nbhlr">
          <node concept="3Mxwew" id="4RsV8qJDdZ4" role="3MwsjC">
            <property role="3MwjfP" value="repackagedClasses.jar" />
          </node>
        </node>
        <node concept="3ygNvl" id="4RsV8qJDdZ6" role="39821P">
          <ref role="3ygNvj" node="60cTC8EBbEi" resolve="module-A.jar" />
        </node>
      </node>
      <node concept="3981dG" id="4RsV8qJDesK" role="39821P">
        <node concept="3_J27D" id="4RsV8qJDesL" role="Nbhlr">
          <node concept="3Mxwew" id="4RsV8qJDesN" role="3MwsjC">
            <property role="3MwjfP" value="zippedJar.zip" />
          </node>
        </node>
        <node concept="3_I8Xc" id="4RsV8qJDesO" role="39821P">
          <ref role="3_I8Xa" node="60cTC8EBbEi" resolve="module-A.jar" />
        </node>
      </node>
      <node concept="398223" id="4RsV8qJCzKs" role="39821P">
        <node concept="3_J27D" id="4RsV8qJCzKt" role="Nbhlr">
          <node concept="3Mxwew" id="4RsV8qJCzKu" role="3MwsjC">
            <property role="3MwjfP" value="Content2" />
          </node>
        </node>
        <node concept="3ygNvl" id="4RsV8qJCzKv" role="39821P">
          <ref role="3ygNvj" node="5DY7s5F3VzN" resolve="result" />
        </node>
      </node>
    </node>
    <node concept="2sgV4H" id="4RsV8qJCzKq" role="1l3spa">
      <ref role="1l3spb" node="3_glsEmoD20" resolve="buildA" />
    </node>
  </node>
  <node concept="1l3spW" id="j1Y_zp_d97">
    <property role="2DA0ip" value="samples" />
    <property role="TrG5h" value="buildY" />
    <property role="turDy" value="buildY.xml" />
    <node concept="398b33" id="j1Y_zp_d9b" role="3989C9">
      <property role="TrG5h" value="module-A-in-Y" />
      <node concept="2GAZfH" id="j1Y_zp_d9o" role="398b2p">
        <node concept="TIC1d" id="j1Y_zp_d9p" role="2GAZfG">
          <node concept="55IIr" id="j1Y_zp_d9q" role="TIC6M">
            <node concept="2Ry0Ak" id="j1Y_zp_d9s" role="iGT6I">
              <property role="2Ry0Am" value="buildA" />
              <node concept="2Ry0Ak" id="j1Y_zp_d9t" role="2Ry0An">
                <property role="2Ry0Am" value="src" />
              </node>
            </node>
          </node>
        </node>
      </node>
      <node concept="25zrj4" id="j1Y_zp_hI_" role="nCB5Z">
        <node concept="2HycW7" id="j1Y_zp_hIA" role="25zrj5">
          <node concept="55IIr" id="j1Y_zp_hIB" role="25ysHA">
            <node concept="2Ry0Ak" id="j1Y_zp_hID" role="iGT6I">
              <property role="2Ry0Am" value="buildA" />
              <node concept="2Ry0Ak" id="j1Y_zp_hIE" role="2Ry0An">
                <property role="2Ry0Am" value="abcde.jar" />
              </node>
            </node>
          </node>
        </node>
      </node>
    </node>
    <node concept="55IIr" id="j1Y_zp_d98" role="auvoZ" />
    <node concept="1l3spV" id="j1Y_zp_d99" role="1l3spN">
      <node concept="3981dx" id="j1Y_zp_dKm" role="39821P">
        <node concept="3_J27D" id="j1Y_zp_dKn" role="Nbhlr">
          <node concept="3Mxwew" id="j1Y_zp_dKo" role="3MwsjC">
            <property role="3MwjfP" value="Aa.jar" />
          </node>
        </node>
        <node concept="Saw0i" id="j1Y_zp_dKp" role="39821P">
          <ref role="Saw0g" node="j1Y_zp_d9b" resolve="module-A-in-Y" />
        </node>
      </node>
      <node concept="3981dG" id="j1Y_zp_ilG" role="39821P">
        <node concept="2HvfSZ" id="j1Y_zp_dKu" role="39821P">
          <node concept="55IIr" id="j1Y_zp_dKv" role="2HvfZ0">
            <node concept="2Ry0Ak" id="j1Y_zp_dKw" role="iGT6I">
              <property role="2Ry0Am" value="buildA" />
            </node>
          </node>
        </node>
        <node concept="3_J27D" id="j1Y_zp_ilH" role="Nbhlr">
          <node concept="3Mxwew" id="j1Y_zp_ilI" role="3MwsjC">
            <property role="3MwjfP" value="AaLibs.zip" />
          </node>
        </node>
      </node>
    </node>
    <node concept="10PD9b" id="j1Y_zp_d9a" role="10PD9s" />
  </node>
  <node concept="1l3spW" id="j1Y_zp_d9u">
    <property role="TrG5h" value="buildZ" />
    <property role="turDy" value="buildZ.xml" />
    <property role="2DA0ip" value="samples" />
    <node concept="398b33" id="j1Y_zp_d9y" role="3989C9">
      <property role="TrG5h" value="module-B-in-Z" />
      <node concept="2GAZfH" id="j1Y_zp_d9D" role="398b2p">
        <node concept="TIC1d" id="j1Y_zp_d9E" role="2GAZfG">
          <node concept="55IIr" id="j1Y_zp_d9F" role="TIC6M">
            <node concept="2Ry0Ak" id="j1Y_zp_d9G" role="iGT6I">
              <property role="2Ry0Am" value="subprojects" />
              <node concept="2Ry0Ak" id="j1Y_zp_d9H" role="2Ry0An">
                <property role="2Ry0Am" value="buildB" />
                <node concept="2Ry0Ak" id="j1Y_zp_d9I" role="2Ry0An">
                  <property role="2Ry0Am" value="moduleB" />
                </node>
              </node>
            </node>
          </node>
        </node>
      </node>
      <node concept="nCB5N" id="j1Y_zp_d9B" role="nCB5Z">
        <property role="1Sh$E4" value="true" />
        <ref role="nCB5Y" node="j1Y_zp_d9b" resolve="module-A-in-Y" />
      </node>
      <node concept="3tkPu6" id="j1Y_zp_eG2" role="nCB5Z">
        <property role="3tkGrO" value="abcde.jar" />
        <property role="3tkHdo" value="true" />
        <node concept="3yqFqT" id="4VmJU8hx0Bu" role="3tkHdp">
          <ref role="3yqFr6" node="j1Y_zp_ilG" resolve="AaLibs.zip" />
        </node>
      </node>
    </node>
    <node concept="55IIr" id="j1Y_zp_d9v" role="auvoZ" />
    <node concept="1l3spV" id="j1Y_zp_d9w" role="1l3spN">
      <node concept="3981dx" id="j1Y_zp_mCN" role="39821P">
        <node concept="3_J27D" id="j1Y_zp_mCO" role="Nbhlr">
          <node concept="3Mxwew" id="j1Y_zp_mCP" role="3MwsjC">
            <property role="3MwjfP" value="modB.jar" />
          </node>
        </node>
        <node concept="Saw0i" id="j1Y_zp_mCQ" role="39821P">
          <ref role="Saw0g" node="j1Y_zp_d9y" resolve="module-B-in-Z" />
        </node>
      </node>
      <node concept="398223" id="j1Y_zp_mCJ" role="39821P">
        <node concept="3_I8Xc" id="j1Y_zp_mCF" role="39821P">
          <ref role="3_I8Xa" node="j1Y_zp_dKm" resolve="Aa.jar" />
        </node>
        <node concept="3_I8Xc" id="j1Y_zp_mCH" role="39821P">
          <ref role="3_I8Xa" node="j1Y_zp_ilG" resolve="AaLibs.zip" />
        </node>
        <node concept="3_J27D" id="j1Y_zp_mCK" role="Nbhlr">
          <node concept="3Mxwew" id="j1Y_zp_mCL" role="3MwsjC">
            <property role="3MwjfP" value="Aa" />
          </node>
        </node>
      </node>
    </node>
    <node concept="2sgV4H" id="j1Y_zp_d9x" role="1l3spa">
      <ref role="1l3spb" node="j1Y_zp_d97" resolve="buildY" />
    </node>
    <node concept="10PD9b" id="j1Y_zp_d9J" role="10PD9s" />
  </node>
  <node concept="1l3spW" id="j1Y_zp_mCA">
    <property role="2DA0ip" value="samples" />
    <property role="TrG5h" value="buildZZ" />
    <property role="turDy" value="buildZZ.xml" />
    <node concept="55IIr" id="j1Y_zp_mCB" role="auvoZ" />
    <node concept="1l3spV" id="j1Y_zp_mCC" role="1l3spN" />
    <node concept="10PD9b" id="j1Y_zp_mCD" role="10PD9s" />
    <node concept="2sgV4H" id="j1Y_zp_mCE" role="1l3spa">
      <ref role="1l3spb" node="j1Y_zp_d9u" resolve="buildZ" />
    </node>
    <node concept="398b33" id="j1Y_zp_mCR" role="3989C9">
      <property role="TrG5h" value="ZZ" />
      <node concept="2GAZfH" id="j1Y_zp_mCS" role="398b2p">
        <node concept="TIC1d" id="j1Y_zp_mCT" role="2GAZfG">
          <node concept="55IIr" id="j1Y_zp_mCU" role="TIC6M">
            <node concept="2Ry0Ak" id="j1Y_zp_mCV" role="iGT6I">
              <property role="2Ry0Am" value="buildD" />
              <node concept="2Ry0Ak" id="j1Y_zp_mCY" role="2Ry0An">
                <property role="2Ry0Am" value="src" />
              </node>
            </node>
          </node>
        </node>
      </node>
      <node concept="nCB5N" id="j1Y_zp_mCX" role="nCB5Z">
        <ref role="nCB5Y" node="j1Y_zp_d9y" resolve="module-B-in-Z" />
      </node>
    </node>
  </node>
</model>
