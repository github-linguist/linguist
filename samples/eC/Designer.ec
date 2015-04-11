import "ide"

class Designer : DesignerBase
{
   ~Designer()
   {
      if(GetActiveDesigner() == this)
      {
         SetActiveDesigner(null);
      }
      if(classDesigner)
         delete classDesigner;
   }

   // *** DesignerBase Implementation ***

   void ModifyCode()
   {
      codeEditor.ModifyCode();
   }

   void UpdateProperties()
   {
      codeEditor.DesignerModifiedObject();
   }

   void CodeAddObject(Instance instance, ObjectInfo * object)
   {
      codeEditor.AddObject(instance, object);
   }

   void SheetAddObject(ObjectInfo object)
   {
      codeEditor.sheet.AddObject(object, object.name, typeData, true); //className, true);
   }

   void AddToolBoxClass(Class _class)
   {
      ((IDEWorkSpace)master).toolBox.AddControl(_class);
   }

   void AddDefaultMethod(Instance instance, Instance classInstance)
   {
      Class _class = instance._class;
      Method defaultMethod = null;

      for( ; _class; _class = _class.base)
      {
         Method method;
         int minID = MAXINT;
         for(method = (Method)_class.methods.first; method; method = (Method)((BTNode)method).next)
         {
            if(method.type == virtualMethod)
            {
               if(!method.dataType)
                  method.dataType = ProcessTypeString(method.dataTypeString, false);
               if(method.vid < minID && (instance == classInstance || (method.dataType.thisClass && eClass_IsDerived(classInstance._class, method.dataType.thisClass.registered))))
               {
                  defaultMethod = method;
                  minID = method.vid;
               }
            }
         }
         if(defaultMethod)
            break;
      }
      codeEditor.AddMethod(defaultMethod);
   }

   bool ObjectContainsCode(ObjectInfo object)
   {
      // Confirmation if control contains code
      if(object.instCode)
      {
         MembersInit members;
         if(object.instCode.members)
         {
            for(members = object.instCode.members->first; members; members = members.next)
            {
               if(members.type == methodMembersInit)
               {
                  //if(!Code_IsFunctionEmpty(members.function))
                  {
                     return true;
                  }
               }
            }
         }
      }
      return false;
   }

   void DeleteObject(ObjectInfo object)
   {
      if(codeEditor)
         codeEditor.DeleteObject(object);
   }

   void RenameObject(ObjectInfo object, const char * name)
   {
      if(object && (name || !object.classDefinition))
         codeEditor.RenameObject(object, name);
   }

   bool FindObject(Instance * object, const char * string)
   {
      ObjectInfo classObject;
      for(classObject = codeEditor.classes.first; classObject; classObject = classObject.next)
      {
         ObjectInfo check;
         if(classObject.name && !strcmp(string, classObject.name))
         {
            *object = classObject.instance;
            break;
         }
         for(check = classObject.instances.first; check; check = check.next)
         {
            if(check.name && !strcmp(string, check.name))
            {
               *object = check.instance;
               break;
            }
         }
         if(check)
            return true;
      }
      return false;
   }

   void SelectObjectFromDesigner(ObjectInfo object)
   {
      codeEditor.SelectObjectFromDesigner(object);
   }

   borderStyle = sizable;
   isActiveClient = true;
   hasVertScroll = true;
   hasHorzScroll = true;
   hasClose = true;
   hasMaximize = true;
   hasMinimize = true;
   text = $"Designer";
   menu = Menu { };
   anchor = Anchor { left = 300, right = 150, top = 0, bottom = 0 };

   ToolBox toolBox;
   CodeEditor codeEditor;

   Menu fileMenu { menu, $"File", f };
   MenuItem fileSaveItem
   {
      fileMenu, $"Save", s, ctrlS;
      bool NotifySelect(MenuItem selection, Modifiers mods)
      {
         return codeEditor.MenuFileSave(selection, mods);
      }
   };
   MenuItem fileSaveAsItem
   {
      fileMenu, $"Save As...", a;
      bool NotifySelect(MenuItem selection, Modifiers mods)
      {
         return codeEditor.MenuFileSaveAs(selection, mods);
      }
   };
   bool debugClosing;

   bool OnClose(bool parentClosing)
   {
      if(!parentClosing)
      {
         if(codeEditor && codeEditor.inUseDebug && !debugClosing)
         {
            debugClosing = true;
            closing = false;
            if(CloseConfirmation(false))
            {
               visible = false;
               if(modifiedDocument)
                  OnFileModified({ modified = true }, null);
            }
            debugClosing = false;
            return false;
         }
         if(codeEditor && !codeEditor.closing && !debugClosing)
         {
            if(!codeEditor.visible)
            {
               if(!codeEditor.Destroy(0))
                  return false;
               else
                  codeEditor = null;
            }
            else
            {
               visible = false;
               return false;
            }
         }
      }
      return true;
   }

   bool OnActivate(bool active, Window previous, bool * goOnWithActivation, bool direct)
   {
      if(active)
      {
         codeEditor.EnsureUpToDate();
         codeEditor.fixCaret = true;
         /*
         if(classDesigner)
            classDesigner.Activate();
         */
      }
      return true;
   }

   bool OnKeyHit(Key key, unichar ch)
   {
      return codeEditor.sheet.OnKeyHit(key, ch);
   }

   watch(modifiedDocument)
   {
      fileSaveItem.disabled = !modifiedDocument && codeEditor.fileName;
   };

   // *** METHODS ACCESSED FROM PROPERTY SHEET/TOOLBOX/CODE EDITOR ***
   void Reset()
   {
      if(classDesigner)
      {
         classDesigner.Reset();
         classDesigner.SelectObject(null, null);
         classDesigner.Destroy(0);
         delete classDesigner;
      }
   }

   void FillToolBox()
   {
      if(this && classDesigner)
         classDesigner.ListToolBoxClasses(this);
   }

   void SelectObject(ObjectInfo object, Instance instance)
   {
      ClassDesignerBase classDesigner = this.classDesigner;
#ifdef _DEBUG
      if(instance && instance._class.module.application != codeEditor.privateModule)
         printf("warning: SelectObject: instance._class.module.application != codeEditor.privateModule\n");
#endif
      if(!classDesigner || !instance || classDesigner._class != (Class)eInstance_GetDesigner(instance))
      {
         if(classDesigner)
         {
            classDesigner.SelectObject(null, null);
            classDesigner.Destroy(0);
            classDesigner = null;
            delete this.classDesigner;
         }
         if(instance)
         {
            this.classDesigner = classDesigner = eInstance_New(eInstance_GetDesigner(instance));
            incref classDesigner;
            //if(!classDesigner.parent)
            {
               classDesigner.parent = this;
               classDesigner.anchor = Anchor { left = 0, right = 0, top = 0, bottom = 0 };
            }
            classDesigner.Create();
         }
      }
      // Call class editor SelectObject
      if(classDesigner)
         classDesigner.SelectObject(object, instance);
   }

   void AddObject()
   {
      // Call class editor AddObject
      if(classDesigner)
         classDesigner.AddObject();
      if(visible)
         Activate();
      else
         codeEditor.Activate();
   }

   void CreateObject(Instance instance, ObjectInfo object, bool isClass, Instance iclass)
   {
      subclass(ClassDesignerBase) designerClass = eInstance_GetDesigner(instance);

      // Call class editor CreateObject
      if(designerClass)
         designerClass.CreateObject(this, instance, object, isClass, iclass);
   }

   void ::PostCreateObject(Instance instance, ObjectInfo object, bool isClass, Instance iclass)
   {
      subclass(ClassDesignerBase) designerClass = eInstance_GetDesigner(instance);

      // Call class editor PostCreateObject
      if(designerClass)
         designerClass.PostCreateObject(instance, object, isClass, iclass);
   }

   void ::DroppedObject(Instance instance, ObjectInfo object, bool isClass, Instance iclass)
   {
      subclass(ClassDesignerBase) designerClass = eInstance_GetDesigner(instance);

      // Call class editor PostCreateObject
      if(designerClass)
         designerClass.DroppedObject(instance, object, isClass, iclass);
   }

   void PrepareTestObject(Instance instance)
   {
      subclass(ClassDesignerBase) designerClass = eInstance_GetDesigner(instance);
      if(designerClass)
         designerClass.PrepareTestObject(this, instance);
   }

   void ::DestroyObject(Instance instance)
   {
      subclass(ClassDesignerBase) designerClass = eInstance_GetDesigner(instance);
      if(designerClass)
         designerClass.DestroyObject(instance);
   }

   void ::FixProperty(Property prop, Instance instance)
   {
      subclass(ClassDesignerBase) designerClass = eInstance_GetDesigner(instance);
      if(designerClass)
         designerClass.FixProperty(prop, instance);
   }
}
