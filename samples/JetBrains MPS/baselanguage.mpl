<?xml version="1.0" encoding="UTF-8"?>
<language namespace="jetbrains.mps.baseLanguage" uuid="f3061a53-9226-4cc5-a443-f952ceaf5816" languageVersion="12" moduleVersion="0">
  <models>
    <modelRoot contentPath="${module}" type="default">
      <sourceRoot location="languageModels" />
    </modelRoot>
  </models>
  <facets>
    <facet type="java">
      <classes generated="true" path="${module}/classes_gen" />
    </facet>
  </facets>
  <accessoryModels>
    <model modelUID="6354ebe7-c22a-4a0f-ac54-50b52ab9b065/java:java.lang(JDK/)" />
  </accessoryModels>
  <generators>
    <generator alias="java" namespace="jetbrains.mps.baseLanguage#1129914002933" uuid="985c8c6a-64b4-486d-a91e-7d4112742556">
      <models>
        <modelRoot contentPath="${module}" type="default">
          <sourceRoot location="generator/java/templates" />
        </modelRoot>
      </models>
      <facets>
        <facet type="java">
          <classes generated="true" path="${module}/generator/classes_gen" />
        </facet>
      </facets>
      <external-templates />
      <dependencies>
        <dependency reexport="false">6ed54515-acc8-4d1e-a16c-9fd6cfe951ea(MPS.Core)</dependency>
        <dependency reexport="false">df345b11-b8c7-4213-ac66-48d2a9b75d88(jetbrains.mps.baseLanguageInternal)</dependency>
        <dependency reexport="false">2d3c70e9-aab2-4870-8d8d-6036800e4103(jetbrains.mps.kernel)</dependency>
        <dependency reexport="false">6354ebe7-c22a-4a0f-ac54-50b52ab9b065(JDK)</dependency>
        <dependency reexport="false">52b81ac7-93fd-4e9e-b972-4995882da6d4(jetbrains.mps.baseLanguage.references.runtime)</dependency>
        <dependency reexport="false">f3061a53-9226-4cc5-a443-f952ceaf5816(jetbrains.mps.baseLanguage)</dependency>
        <dependency reexport="false">8865b7a8-5271-43d3-884c-6fd1d9cfdd34(MPS.OpenAPI)</dependency>
      </dependencies>
      <languageVersions>
        <language slang="l:f3061a53-9226-4cc5-a443-f952ceaf5816:jetbrains.mps.baseLanguage" version="12" />
        <language slang="l:fd392034-7849-419d-9071-12563d152375:jetbrains.mps.baseLanguage.closures" version="0" />
        <language slang="l:83888646-71ce-4f1c-9c53-c54016f6ad4f:jetbrains.mps.baseLanguage.collections" version="1" />
        <language slang="l:f2801650-65d5-424e-bb1b-463a8781b786:jetbrains.mps.baseLanguage.javadoc" version="2" />
        <language slang="l:760a0a8c-eabb-4521-8bfd-65db761a9ba3:jetbrains.mps.baseLanguage.logging" version="0" />
        <language slang="l:a247e09e-2435-45ba-b8d2-07e93feba96a:jetbrains.mps.baseLanguage.tuples" version="0" />
        <language slang="l:df345b11-b8c7-4213-ac66-48d2a9b75d88:jetbrains.mps.baseLanguageInternal" version="0" />
        <language slang="l:ceab5195-25ea-4f22-9b92-103b95ca8c0c:jetbrains.mps.lang.core" version="2" />
        <language slang="l:b401a680-8325-4110-8fd3-84331ff25bef:jetbrains.mps.lang.generator" version="4" />
        <language slang="l:d7706f63-9be2-479c-a3da-ae92af1e64d5:jetbrains.mps.lang.generator.generationContext" version="2" />
        <language slang="l:289fcc83-6543-41e8-a5ca-768235715ce4:jetbrains.mps.lang.generator.generationParameters" version="0" />
        <language slang="l:446c26eb-2b7b-4bf0-9b35-f83fa582753e:jetbrains.mps.lang.modelapi" version="0" />
        <language slang="l:3a13115c-633c-4c5c-bbcc-75c4219e9555:jetbrains.mps.lang.quotation" version="5" />
        <language slang="l:13744753-c81f-424a-9c1b-cf8943bf4e86:jetbrains.mps.lang.sharedConcepts" version="0" />
        <language slang="l:7866978e-a0f0-4cc7-81bc-4d213d9375e1:jetbrains.mps.lang.smodel" version="19" />
        <language slang="l:c72da2b9-7cce-4447-8389-f407dc1158b7:jetbrains.mps.lang.structure" version="9" />
        <language slang="l:c7fb639f-be78-4307-89b0-b5959c3fa8c8:jetbrains.mps.lang.text" version="0" />
        <language slang="l:9ded098b-ad6a-4657-bfd9-48636cfe8bc3:jetbrains.mps.lang.traceable" version="0" />
        <language slang="l:7a5dda62-9140-4668-ab76-d5ed1746f2b2:jetbrains.mps.lang.typesystem" version="5" />
      </languageVersions>
      <dependencyVersions>
        <module reference="3f233e7f-b8a6-46d2-a57f-795d56775243(Annotations)" version="0" />
        <module reference="6354ebe7-c22a-4a0f-ac54-50b52ab9b065(JDK)" version="0" />
        <module reference="6ed54515-acc8-4d1e-a16c-9fd6cfe951ea(MPS.Core)" version="0" />
        <module reference="8865b7a8-5271-43d3-884c-6fd1d9cfdd34(MPS.OpenAPI)" version="0" />
        <module reference="f3061a53-9226-4cc5-a443-f952ceaf5816(jetbrains.mps.baseLanguage)" version="0" />
        <module reference="985c8c6a-64b4-486d-a91e-7d4112742556(jetbrains.mps.baseLanguage#1129914002933)" version="0" />
        <module reference="52b81ac7-93fd-4e9e-b972-4995882da6d4(jetbrains.mps.baseLanguage.references.runtime)" version="0" />
        <module reference="e39e4a59-8cb6-498e-860e-8fa8361c0d90(jetbrains.mps.baseLanguage.scopes)" version="0" />
        <module reference="df345b11-b8c7-4213-ac66-48d2a9b75d88(jetbrains.mps.baseLanguageInternal)" version="0" />
        <module reference="2d3c70e9-aab2-4870-8d8d-6036800e4103(jetbrains.mps.kernel)" version="0" />
        <module reference="ceab5195-25ea-4f22-9b92-103b95ca8c0c(jetbrains.mps.lang.core)" version="0" />
        <module reference="9ded098b-ad6a-4657-bfd9-48636cfe8bc3(jetbrains.mps.lang.traceable)" version="0" />
      </dependencyVersions>
      <mapping-priorities>
        <mapping-priority-rule kind="strictly_after">
          <greater-priority-mapping>
            <generator generatorUID="985c8c6a-64b4-486d-a91e-7d4112742556(jetbrains.mps.baseLanguage#1129914002933)" />
            <external-mapping>
              <mapping-node modelUID="r:564046f4-0d85-4d4e-9463-a44226941947(jetbrains.mps.baseLanguage.generator.java.references@generator)" nodeID="3825148134789453017" />
            </external-mapping>
          </greater-priority-mapping>
          <lesser-priority-mapping>
            <generator generatorUID="985c8c6a-64b4-486d-a91e-7d4112742556(jetbrains.mps.baseLanguage#1129914002933)" />
            <external-mapping>
              <mapping-node modelUID="r:564046f4-0d85-4d4e-9463-a44226941947(jetbrains.mps.baseLanguage.generator.java.references@generator)" nodeID="2770522517496730317" />
            </external-mapping>
          </lesser-priority-mapping>
        </mapping-priority-rule>
        <mapping-priority-rule kind="strictly_together">
          <greater-priority-mapping>
            <generator generatorUID="985c8c6a-64b4-486d-a91e-7d4112742556(jetbrains.mps.baseLanguage#1129914002933)" />
            <external-mapping>
              <mapping-node modelUID="r:00000000-0000-4000-0000-011c895902cd(jetbrains.mps.baseLanguage.generator.java.main@generator)" nodeID="1201381967720" />
            </external-mapping>
          </greater-priority-mapping>
          <lesser-priority-mapping>
            <generator generatorUID="985c8c6a-64b4-486d-a91e-7d4112742556(jetbrains.mps.baseLanguage#1129914002933)" />
            <external-mapping>
              <mapping-set>
                <mapping-set-element>
                  <mapping-node modelUID="r:b4d7d620-6723-4aa2-856b-118497e84e9e(jetbrains.mps.baseLanguage.generator.java.strings@generator)" nodeID="*" />
                </mapping-set-element>
                <mapping-set-element>
                  <mapping-node modelUID="r:00000000-0000-4000-0000-011c895902cb(jetbrains.mps.baseLanguage.generator.java.closures@generator)" nodeID="*" />
                </mapping-set-element>
              </mapping-set>
            </external-mapping>
          </lesser-priority-mapping>
        </mapping-priority-rule>
        <mapping-priority-rule kind="strictly_after">
          <greater-priority-mapping>
            <generator generatorUID="985c8c6a-64b4-486d-a91e-7d4112742556(jetbrains.mps.baseLanguage#1129914002933)" />
            <external-mapping>
              <mapping-node modelUID="r:00000000-0000-4000-0000-011c895902cd(jetbrains.mps.baseLanguage.generator.java.main@generator)" nodeID="1201381967720" />
            </external-mapping>
          </greater-priority-mapping>
          <lesser-priority-mapping>
            <generator generatorUID="985c8c6a-64b4-486d-a91e-7d4112742556(jetbrains.mps.baseLanguage#1129914002933)" />
            <external-mapping>
              <mapping-node modelUID="r:564046f4-0d85-4d4e-9463-a44226941947(jetbrains.mps.baseLanguage.generator.java.references@generator)" nodeID="3825148134789453017" />
            </external-mapping>
          </lesser-priority-mapping>
        </mapping-priority-rule>
        <mapping-priority-rule kind="strictly_after">
          <greater-priority-mapping>
            <generator generatorUID="985c8c6a-64b4-486d-a91e-7d4112742556(jetbrains.mps.baseLanguage#1129914002933)" />
            <external-mapping>
              <mapping-node modelUID="r:564046f4-0d85-4d4e-9463-a44226941947(jetbrains.mps.baseLanguage.generator.java.references@generator)" nodeID="*" />
            </external-mapping>
          </greater-priority-mapping>
          <lesser-priority-mapping>
            <generator generatorUID="985c8c6a-64b4-486d-a91e-7d4112742556(jetbrains.mps.baseLanguage#1129914002933)" />
            <external-mapping>
              <mapping-node modelUID="r:8b84f2f2-3f35-4698-bc08-fafea3caa1b5(jetbrains.mps.baseLanguage.generator.java.properties@generator)" nodeID="*" />
            </external-mapping>
          </lesser-priority-mapping>
        </mapping-priority-rule>
      </mapping-priorities>
    </generator>
  </generators>
  <sourcePath />
  <dependencies>
    <dependency reexport="true">2d3c70e9-aab2-4870-8d8d-6036800e4103(jetbrains.mps.kernel)</dependency>
    <dependency reexport="false">ceab5195-25ea-4f22-9b92-103b95ca8c0c(jetbrains.mps.lang.core)</dependency>
    <dependency reexport="false">c72da2b9-7cce-4447-8389-f407dc1158b7(jetbrains.mps.lang.structure)</dependency>
    <dependency reexport="false">af65afd8-f0dd-4942-87d9-63a55f2a9db1(jetbrains.mps.lang.behavior)</dependency>
    <dependency reexport="false">6ed54515-acc8-4d1e-a16c-9fd6cfe951ea(MPS.Core)</dependency>
    <dependency reexport="false">d7eb0a2a-bd50-4576-beae-e4a89db35f20(jetbrains.mps.lang.scopes.runtime)</dependency>
    <dependency reexport="true">e39e4a59-8cb6-498e-860e-8fa8361c0d90(jetbrains.mps.baseLanguage.scopes)</dependency>
    <dependency reexport="false">443f4c36-fcf5-4eb6-9500-8d06ed259e3e(jetbrains.mps.baseLanguage.classifiers)</dependency>
    <dependency reexport="false">3f233e7f-b8a6-46d2-a57f-795d56775243(Annotations)</dependency>
    <dependency reexport="false">c7d01124-66d5-486d-8b50-7fdccb60b839(jetbrains.mps.baseLanguage.util)</dependency>
    <dependency reexport="false">7a5dda62-9140-4668-ab76-d5ed1746f2b2(jetbrains.mps.lang.typesystem)</dependency>
    <dependency reexport="false">3a13115c-633c-4c5c-bbcc-75c4219e9555(jetbrains.mps.lang.quotation)</dependency>
    <dependency reexport="false">a247e09e-2435-45ba-b8d2-07e93feba96a(jetbrains.mps.baseLanguage.tuples)</dependency>
    <dependency reexport="false">af19274f-5f89-42dd-8f3c-c9932448f7f2(jetbrains.mps.analyzers.runtime)</dependency>
    <dependency reexport="false">f2801650-65d5-424e-bb1b-463a8781b786(jetbrains.mps.baseLanguage.javadoc)</dependency>
    <dependency reexport="false">2af156ab-65c1-4a62-bd0d-ea734f71eab6(jetbrains.mps.dataFlow.runtime)</dependency>
    <dependency reexport="false">1ed103c3-3aa6-49b7-9c21-6765ee11f224(MPS.Editor)</dependency>
    <dependency reexport="false">20c6e580-bdc5-4067-8049-d7e3265a86de(jetbrains.mps.typesystemEngine)</dependency>
    <dependency reexport="true">6354ebe7-c22a-4a0f-ac54-50b52ab9b065(JDK)</dependency>
    <dependency reexport="false">83888646-71ce-4f1c-9c53-c54016f6ad4f(jetbrains.mps.baseLanguage.collections)</dependency>
    <dependency reexport="false">8865b7a8-5271-43d3-884c-6fd1d9cfdd34(MPS.OpenAPI)</dependency>
    <dependency reexport="false">a0c108f0-1637-416e-a249-3effbaa4c998(jetbrains.mps.baseLanguage.search)</dependency>
    <dependency reexport="false">34e84b8f-afa8-4364-abcd-a279fddddbe7(jetbrains.mps.editor.runtime)</dependency>
    <dependency reexport="false">f3061a53-9226-4cc5-a443-f952ceaf5816(jetbrains.mps.baseLanguage)</dependency>
    <dependency reexport="false">528ff3b9-5fc4-40dd-931f-c6ce3650640e(jetbrains.mps.lang.migration.runtime)</dependency>
    <dependency reexport="false">ed6d7656-532c-4bc2-81d1-af945aeb8280(jetbrains.mps.baseLanguage.blTypes)</dependency>
    <dependency reexport="false">90746344-04fd-4286-97d5-b46ae6a81709(jetbrains.mps.lang.migration)</dependency>
    <dependency reexport="false">52b81ac7-93fd-4e9e-b972-4995882da6d4(jetbrains.mps.baseLanguage.references.runtime)</dependency>
    <dependency reexport="false">c7fb639f-be78-4307-89b0-b5959c3fa8c8(jetbrains.mps.lang.text)</dependency>
    <dependency reexport="false">8d29d73f-ed99-4652-ae0a-083cdfe53c34(jetbrains.mps.ide.platform)</dependency>
  </dependencies>
  <languageVersions>
    <language slang="l:f3061a53-9226-4cc5-a443-f952ceaf5816:jetbrains.mps.baseLanguage" version="12" />
    <language slang="l:ed6d7656-532c-4bc2-81d1-af945aeb8280:jetbrains.mps.baseLanguage.blTypes" version="0" />
    <language slang="l:774bf8a0-62e5-41e1-af63-f4812e60e48b:jetbrains.mps.baseLanguage.checkedDots" version="0" />
    <language slang="l:443f4c36-fcf5-4eb6-9500-8d06ed259e3e:jetbrains.mps.baseLanguage.classifiers" version="0" />
    <language slang="l:fd392034-7849-419d-9071-12563d152375:jetbrains.mps.baseLanguage.closures" version="0" />
    <language slang="l:83888646-71ce-4f1c-9c53-c54016f6ad4f:jetbrains.mps.baseLanguage.collections" version="1" />
    <language slang="l:f2801650-65d5-424e-bb1b-463a8781b786:jetbrains.mps.baseLanguage.javadoc" version="2" />
    <language slang="l:c7d5b9dd-a05f-4be2-bc73-f2e16994cc67:jetbrains.mps.baseLanguage.lightweightdsl" version="1" />
    <language slang="l:760a0a8c-eabb-4521-8bfd-65db761a9ba3:jetbrains.mps.baseLanguage.logging" version="0" />
    <language slang="l:daafa647-f1f7-4b0b-b096-69cd7c8408c0:jetbrains.mps.baseLanguage.regexp" version="0" />
    <language slang="l:a247e09e-2435-45ba-b8d2-07e93feba96a:jetbrains.mps.baseLanguage.tuples" version="0" />
    <language slang="l:aee9cad2-acd4-4608-aef2-0004f6a1cdbd:jetbrains.mps.lang.actions" version="4" />
    <language slang="l:af65afd8-f0dd-4942-87d9-63a55f2a9db1:jetbrains.mps.lang.behavior" version="2" />
    <language slang="l:3f4bc5f5-c6c1-4a28-8b10-c83066ffa4a1:jetbrains.mps.lang.constraints" version="6" />
    <language slang="l:e51810c5-7308-4642-bcb6-469e61b5dd18:jetbrains.mps.lang.constraints.msg.specification" version="0" />
    <language slang="l:47257bf3-78d3-470b-89d9-8c3261a61d15:jetbrains.mps.lang.constraints.rules" version="0" />
    <language slang="l:5dae8159-ab99-46bb-a40d-0cee30ee7018:jetbrains.mps.lang.constraints.rules.kinds" version="0" />
    <language slang="l:134c38d4-e3af-4d9e-b069-1c7df0a4005d:jetbrains.mps.lang.constraints.rules.skeleton" version="0" />
    <language slang="l:3ad5badc-1d9c-461c-b7b1-fa2fcd0a0ae7:jetbrains.mps.lang.context" version="0" />
    <language slang="l:ea3159bf-f48e-4720-bde2-86dba75f0d34:jetbrains.mps.lang.context.defs" version="0" />
    <language slang="l:ceab5195-25ea-4f22-9b92-103b95ca8c0c:jetbrains.mps.lang.core" version="2" />
    <language slang="l:7fa12e9c-b949-4976-b4fa-19accbc320b4:jetbrains.mps.lang.dataFlow" version="1" />
    <language slang="l:97a52717-898f-4598-8150-573d9fd03868:jetbrains.mps.lang.dataFlow.analyzers" version="0" />
    <language slang="l:f4ad079d-bc71-4ffb-9600-9328705cf998:jetbrains.mps.lang.descriptor" version="0" />
    <language slang="l:18bc6592-03a6-4e29-a83a-7ff23bde13ba:jetbrains.mps.lang.editor" version="14" />
    <language slang="l:64d34fcd-ad02-4e73-aff8-a581124c2e30:jetbrains.mps.lang.findUsages" version="0" />
    <language slang="l:d7a92d38-f7db-40d0-8431-763b0c3c9f20:jetbrains.mps.lang.intentions" version="1" />
    <language slang="l:90746344-04fd-4286-97d5-b46ae6a81709:jetbrains.mps.lang.migration" version="2" />
    <language slang="l:446c26eb-2b7b-4bf0-9b35-f83fa582753e:jetbrains.mps.lang.modelapi" version="0" />
    <language slang="l:d4615e3b-d671-4ba9-af01-2b78369b0ba7:jetbrains.mps.lang.pattern" version="2" />
    <language slang="l:3a13115c-633c-4c5c-bbcc-75c4219e9555:jetbrains.mps.lang.quotation" version="5" />
    <language slang="l:3ecd7c84-cde3-45de-886c-135ecc69b742:jetbrains.mps.lang.refactoring" version="0" />
    <language slang="l:982eb8df-2c96-4bd7-9963-11712ea622e5:jetbrains.mps.lang.resources" version="2" />
    <language slang="l:b3551702-269c-4f05-ba61-58060cef4292:jetbrains.mps.lang.rulesAndMessages" version="0" />
    <language slang="l:d8f591ec-4d86-4af2-9f92-a9e93c803ffa:jetbrains.mps.lang.scopes" version="0" />
    <language slang="l:0eddeefa-c2d6-4437-bc2c-de50fd4ce470:jetbrains.mps.lang.script" version="1" />
    <language slang="l:13744753-c81f-424a-9c1b-cf8943bf4e86:jetbrains.mps.lang.sharedConcepts" version="0" />
    <language slang="l:7866978e-a0f0-4cc7-81bc-4d213d9375e1:jetbrains.mps.lang.smodel" version="19" />
    <language slang="l:1a8554c4-eb84-43ba-8c34-6f0d90c6e75a:jetbrains.mps.lang.smodel.query" version="3" />
    <language slang="l:c72da2b9-7cce-4447-8389-f407dc1158b7:jetbrains.mps.lang.structure" version="9" />
    <language slang="l:c7fb639f-be78-4307-89b0-b5959c3fa8c8:jetbrains.mps.lang.text" version="0" />
    <language slang="l:b83431fe-5c8f-40bc-8a36-65e25f4dd253:jetbrains.mps.lang.textGen" version="1" />
    <language slang="l:9ded098b-ad6a-4657-bfd9-48636cfe8bc3:jetbrains.mps.lang.traceable" version="0" />
    <language slang="l:7a5dda62-9140-4668-ab76-d5ed1746f2b2:jetbrains.mps.lang.typesystem" version="5" />
  </languageVersions>
  <dependencyVersions>
    <module reference="3f233e7f-b8a6-46d2-a57f-795d56775243(Annotations)" version="0" />
    <module reference="6354ebe7-c22a-4a0f-ac54-50b52ab9b065(JDK)" version="0" />
    <module reference="6ed54515-acc8-4d1e-a16c-9fd6cfe951ea(MPS.Core)" version="0" />
    <module reference="1ed103c3-3aa6-49b7-9c21-6765ee11f224(MPS.Editor)" version="0" />
    <module reference="498d89d2-c2e9-11e2-ad49-6cf049e62fe5(MPS.IDEA)" version="0" />
    <module reference="8865b7a8-5271-43d3-884c-6fd1d9cfdd34(MPS.OpenAPI)" version="0" />
    <module reference="742f6602-5a2f-4313-aa6e-ae1cd4ffdc61(MPS.Platform)" version="0" />
    <module reference="af19274f-5f89-42dd-8f3c-c9932448f7f2(jetbrains.mps.analyzers.runtime)" version="0" />
    <module reference="f3061a53-9226-4cc5-a443-f952ceaf5816(jetbrains.mps.baseLanguage)" version="0" />
    <module reference="ed6d7656-532c-4bc2-81d1-af945aeb8280(jetbrains.mps.baseLanguage.blTypes)" version="0" />
    <module reference="443f4c36-fcf5-4eb6-9500-8d06ed259e3e(jetbrains.mps.baseLanguage.classifiers)" version="0" />
    <module reference="fd392034-7849-419d-9071-12563d152375(jetbrains.mps.baseLanguage.closures)" version="0" />
    <module reference="83888646-71ce-4f1c-9c53-c54016f6ad4f(jetbrains.mps.baseLanguage.collections)" version="0" />
    <module reference="f2801650-65d5-424e-bb1b-463a8781b786(jetbrains.mps.baseLanguage.javadoc)" version="0" />
    <module reference="c7d5b9dd-a05f-4be2-bc73-f2e16994cc67(jetbrains.mps.baseLanguage.lightweightdsl)" version="0" />
    <module reference="52b81ac7-93fd-4e9e-b972-4995882da6d4(jetbrains.mps.baseLanguage.references.runtime)" version="0" />
    <module reference="e39e4a59-8cb6-498e-860e-8fa8361c0d90(jetbrains.mps.baseLanguage.scopes)" version="0" />
    <module reference="a0c108f0-1637-416e-a249-3effbaa4c998(jetbrains.mps.baseLanguage.search)" version="0" />
    <module reference="a247e09e-2435-45ba-b8d2-07e93feba96a(jetbrains.mps.baseLanguage.tuples)" version="0" />
    <module reference="c7d01124-66d5-486d-8b50-7fdccb60b839(jetbrains.mps.baseLanguage.util)" version="0" />
    <module reference="fdaaf35f-8ee3-4c37-b09d-9efaeaaa7a41(jetbrains.mps.core.tool.environment)" version="0" />
    <module reference="2af156ab-65c1-4a62-bd0d-ea734f71eab6(jetbrains.mps.dataFlow.runtime)" version="0" />
    <module reference="34e84b8f-afa8-4364-abcd-a279fddddbe7(jetbrains.mps.editor.runtime)" version="0" />
    <module reference="8d29d73f-ed99-4652-ae0a-083cdfe53c34(jetbrains.mps.ide.platform)" version="0" />
    <module reference="2d3c70e9-aab2-4870-8d8d-6036800e4103(jetbrains.mps.kernel)" version="0" />
    <module reference="af65afd8-f0dd-4942-87d9-63a55f2a9db1(jetbrains.mps.lang.behavior)" version="0" />
    <module reference="d936855b-48da-4812-a8a0-2bfddd633ac5(jetbrains.mps.lang.behavior.api)" version="0" />
    <module reference="ceab5195-25ea-4f22-9b92-103b95ca8c0c(jetbrains.mps.lang.core)" version="0" />
    <module reference="a9e4c532-c5f5-4bb7-99ef-42abb73bbb70(jetbrains.mps.lang.descriptor.aspects)" version="0" />
    <module reference="90746344-04fd-4286-97d5-b46ae6a81709(jetbrains.mps.lang.migration)" version="0" />
    <module reference="528ff3b9-5fc4-40dd-931f-c6ce3650640e(jetbrains.mps.lang.migration.runtime)" version="0" />
    <module reference="446c26eb-2b7b-4bf0-9b35-f83fa582753e(jetbrains.mps.lang.modelapi)" version="0" />
    <module reference="3a13115c-633c-4c5c-bbcc-75c4219e9555(jetbrains.mps.lang.quotation)" version="0" />
    <module reference="d7eb0a2a-bd50-4576-beae-e4a89db35f20(jetbrains.mps.lang.scopes.runtime)" version="0" />
    <module reference="7866978e-a0f0-4cc7-81bc-4d213d9375e1(jetbrains.mps.lang.smodel)" version="1" />
    <module reference="c72da2b9-7cce-4447-8389-f407dc1158b7(jetbrains.mps.lang.structure)" version="0" />
    <module reference="c7fb639f-be78-4307-89b0-b5959c3fa8c8(jetbrains.mps.lang.text)" version="0" />
    <module reference="9ded098b-ad6a-4657-bfd9-48636cfe8bc3(jetbrains.mps.lang.traceable)" version="0" />
    <module reference="7a5dda62-9140-4668-ab76-d5ed1746f2b2(jetbrains.mps.lang.typesystem)" version="0" />
    <module reference="8fe4c62a-2020-4ff4-8eda-f322a55bdc9f(jetbrains.mps.refactoring.runtime)" version="0" />
    <module reference="20c6e580-bdc5-4067-8049-d7e3265a86de(jetbrains.mps.typesystemEngine)" version="0" />
  </dependencyVersions>
  <runtime>
    <dependency reexport="false">6354ebe7-c22a-4a0f-ac54-50b52ab9b065(JDK)</dependency>
    <dependency reexport="false">52b81ac7-93fd-4e9e-b972-4995882da6d4(jetbrains.mps.baseLanguage.references.runtime)</dependency>
  </runtime>
  <extendedLanguages>
    <extendedLanguage>ceab5195-25ea-4f22-9b92-103b95ca8c0c(jetbrains.mps.lang.core)</extendedLanguage>
    <extendedLanguage>9ded098b-ad6a-4657-bfd9-48636cfe8bc3(jetbrains.mps.lang.traceable)</extendedLanguage>
  </extendedLanguages>
</language>
