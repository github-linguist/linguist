// ------------------------------------------------------------------------------
// Copyright (c) 2011 GeometryFactory (FRANCE)
// Distributed under the Boost Software License, Version 1.0. (See accompany-
// ing file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
// ------------------------------------------------------------------------------ 


%module (package="CGAL") CGAL_AABB_tree

%include "SWIG_CGAL/common.i"
Decl_void_type()

SWIG_CGAL_add_java_loadLibrary(CGAL_AABB_tree)
SWIG_CGAL_package_common()

%import  "SWIG_CGAL/Common/Macros.h"
%include "SWIG_CGAL/Common/Iterator.h"
%include "SWIG_CGAL/Common/Optional.h"
%import  "SWIG_CGAL/Kernel/CGAL_Kernel.i"

//include files
%{
  #define CGAL_INTERSECTION_VERSION 1
  #include  <SWIG_CGAL/Kernel/typedefs.h>
  #include  <SWIG_CGAL/Kernel/Point_3.h>
  #include  <SWIG_CGAL/Kernel/Triangle_3.h>
  #include  <SWIG_CGAL/Kernel/Segment_3.h>
  #include  <SWIG_CGAL/Polyhedron_3/all_includes.h>
  #include  <SWIG_CGAL/AABB_tree/all_includes.h>
  #include  <SWIG_CGAL/AABB_tree/Object.h>
%}

//import definitions of Polyhedron objects
%import "SWIG_CGAL/Polyhedron_3/CGAL_Polyhedron_3.i"

//definitions
%include "SWIG_CGAL/AABB_tree/AABB_tree.h"

%pragma(java) jniclassimports=%{
  import CGAL.Kernel.Triangle_3; import CGAL.Kernel.Segment_3; import CGAL.Kernel.Plane_3; import CGAL.Kernel.Ray_3; import CGAL.Kernel.Point_3;
  import CGAL.Polyhedron_3.Polyhedron_3_Halfedge_handle; import CGAL.Polyhedron_3.Polyhedron_3_Facet_handle; import java.util.Iterator; import java.util.Collection;
%}

//local Object class: we cannot use the class from Kernel module as CGAL::Object uses RTTI
#ifdef SWIG_CGAL_AABB_tree_MODULE
%include "SWIG_CGAL/Common/Object.i"
#endif

//import Polyhedron_3 wrapper types
SWIG_CGAL_import_Polyhedron_3_Facet_handle_SWIG_wrapper
SWIG_CGAL_import_Polyhedron_3_Halfedge_handle_SWIG_wrapper


%include "std_pair.i"
//Point_and_primitive_id
%typemap(javaimports)       std::pair<Point_3,Polyhedron_3_Facet_handle_SWIG_wrapper > %{import CGAL.Kernel.Point_3; import CGAL.Polyhedron_3.Polyhedron_3_Facet_handle;%}
SWIG_CGAL_declare_identifier_of_template_class(Point_and_Polyhedron_3_Facet_handle,std::pair<Point_3,Polyhedron_3_Facet_handle_SWIG_wrapper >)
%typemap(javaimports)       std::pair<Point_3,Polyhedron_3_Halfedge_handle_SWIG_wrapper > %{import CGAL.Kernel.Point_3; import CGAL.Polyhedron_3.Polyhedron_3_Halfedge_handle;%}
SWIG_CGAL_declare_identifier_of_template_class(Point_and_Polyhedron_3_Halfedge_handle,std::pair<Point_3,Polyhedron_3_Halfedge_handle_SWIG_wrapper >)
%typemap(javaimports)       std::pair<Point_3,int > %{import CGAL.Kernel.Point_3;%}
SWIG_CGAL_declare_identifier_of_template_class(Point_and_Integer,std::pair<Point_3,int >)
//Object_and_primitive_id
%typemap(javaimports)       std::pair<Object,Polyhedron_3_Facet_handle_SWIG_wrapper > %{import CGAL.Polyhedron_3.Polyhedron_3_Facet_handle;%}
SWIG_CGAL_declare_identifier_of_template_class(Object_and_Polyhedron_3_Facet_handle,std::pair<Object,Polyhedron_3_Facet_handle_SWIG_wrapper >)
%typemap(javaimports)       std::pair<Object,Polyhedron_3_Halfedge_handle_SWIG_wrapper > %{import CGAL.Polyhedron_3.Polyhedron_3_Halfedge_handle;%}
SWIG_CGAL_declare_identifier_of_template_class(Object_and_Polyhedron_3_Halfedge_handle,std::pair<Object,Polyhedron_3_Halfedge_handle_SWIG_wrapper >)
%typemap(javaimports)       std::pair<Object,int > %{import CGAL.Polyhedron_3.Polyhedron_3_Halfedge_handle;%}
SWIG_CGAL_declare_identifier_of_template_class(Object_and_Integer,std::pair<Object,int >)
//Optional<primitive_id>
%typemap(javaimports)       Optional<Polyhedron_3_Halfedge_handle_SWIG_wrapper > %{import CGAL.Polyhedron_3.Polyhedron_3_Halfedge_handle;%}
SWIG_CGAL_declare_identifier_of_template_class(Optional_Polyhedron_3_Halfedge_handle,Optional<Polyhedron_3_Halfedge_handle_SWIG_wrapper >);
%typemap(javaimports)       Optional<Polyhedron_3_Facet_handle_SWIG_wrapper > %{import CGAL.Polyhedron_3.Polyhedron_3_Facet_handle;%}
SWIG_CGAL_declare_identifier_of_template_class(Optional_Polyhedron_3_Facet_handle,Optional<Polyhedron_3_Facet_handle_SWIG_wrapper >)
SWIG_CGAL_declare_identifier_of_template_class(Optional_Integer,Optional< int >);
//Optional<Object_and_primitive_id>
SWIG_CGAL_declare_identifier_of_template_class(Optional_Object_and_Polyhedron_3_Halfedge_handle,Optional< std::pair<Object,Polyhedron_3_Halfedge_handle_SWIG_wrapper > >)
SWIG_CGAL_declare_identifier_of_template_class(Optional_Object_and_Polyhedron_3_Facet_handle,Optional< std::pair<Object,Polyhedron_3_Facet_handle_SWIG_wrapper > >)
SWIG_CGAL_declare_identifier_of_template_class(Optional_Object_and_Integer,Optional< std::pair<Object,int > >)


#if !SWIG_CGAL_NON_SUPPORTED_TARGET_LANGUAGE
SWIG_CGAL_input_iterator_typemap_in(Primitive_iterator_helper< Polyhedron_3_Facet_handle_SWIG_wrapper_for_typemap >::input,Polyhedron_3_Facet_handle_SWIG_wrapper,Polyhedron_3_Facet_handle,Polyhedron_3_Facet_handle_SWIG_wrapper::cpp_base,SWIGTYPE_p_SWIG_Polyhedron_3__CGAL_Facet_handleT_Polyhedron_3__t,"(LCGAL/Polyhedron_3/Polyhedron_3_Facet_handle;)J",rebuild)
SWIG_CGAL_input_iterator_typemap_in(Primitive_iterator_helper< Polyhedron_3_Halfedge_handle_SWIG_wrapper_for_typemap >::input,Polyhedron_3_Halfedge_handle_SWIG_wrapper,Polyhedron_3_Halfedge_handle,Polyhedron_3_Halfedge_handle_SWIG_wrapper::cpp_base,SWIGTYPE_p_SWIG_Polyhedron_3__CGAL_Halfedge_handleT_Polyhedron_3__t,"(LCGAL/Polyhedron_3/Polyhedron_3_Halfedge_handle;)J",rebuild)
SWIG_CGAL_input_iterator_typemap_in(Primitive_iterator_helper< Triangle_3 >::input,Triangle_3,Triangle_3,Triangle_3::cpp_base,SWIGTYPE_p_Triangle_3,"(LCGAL/Kernel/Triangle_3;)J",rebuild)
SWIG_CGAL_input_iterator_typemap_in(Primitive_iterator_helper< Segment_3 >::input,Segment_3,Segment_3,Segment_3::cpp_base,SWIGTYPE_p_Segment_3,"(LCGAL/Kernel/Segment_3;)J",rebuild)
#ifdef SWIGPYTHON
SWIG_CGAL_input_iterator_typemap_in_python_extra_function(AABB_tree_wrapper::AABB_tree_wrapper)
#endif
SWIG_CGAL_input_iterator_typemap_in(Point_range,Point_3,Point_3,Point_3::cpp_base,SWIGTYPE_p_Point_3,"(LCGAL/Kernel/Point_3;)J",accelerate_distance_queries)
#else //!SWIG_CGAL_NON_SUPPORTED_TARGET_LANGUAGE
SWIG_CGAL_declare_identifier_of_template_class(Polyhedron_3_Facet_handle_input_iterator,Generic_input_iterator< Polyhedron_3_Facet_handle_SWIG_wrapper >)
SWIG_CGAL_declare_identifier_of_template_class(Polyhedron_3_Halfedge_handle_input_iterator,Generic_input_iterator< Polyhedron_3_Halfedge_handle_SWIG_wrapper >)
#endif //!SWIG_CGAL_NON_SUPPORTED_TARGET_LANGUAGE

#if !SWIG_CGAL_NON_SUPPORTED_TARGET_LANGUAGE
//intersected primitive output iterator
SWIG_CGAL_output_iterator_typemap_in(Primitive_iterator_helper<Polyhedron_3_Facet_handle_SWIG_wrapper_for_typemap >::output,Polyhedron_3_Facet_handle_SWIG_wrapper ,Polyhedron_3_Facet_handle,Polyhedron_3_Facet_handle_SWIG_wrapper ::cpp_base,SWIGTYPE_p_SWIG_Polyhedron_3__CGAL_Facet_handleT_Polyhedron_3__t,"LCGAL/Polyhedron_3/Polyhedron_3_Facet_handle;")
SWIG_CGAL_output_iterator_typemap_in(Primitive_iterator_helper<Polyhedron_3_Halfedge_handle_SWIG_wrapper_for_typemap >::output,Polyhedron_3_Halfedge_handle_SWIG_wrapper,Polyhedron_3_Halfedge_handle,Polyhedron_3_Halfedge_handle_SWIG_wrapper::cpp_base,SWIGTYPE_p_SWIG_Polyhedron_3__CGAL_Halfedge_handleT_Polyhedron_3__t,"LCGAL/Polyhedron_3/Polyhedron_3_Halfedge_handle;")
SWIG_CGAL_output_iterator_typemap_in(Primitive_iterator_helper< int >::output,int,Integer,int,swig_types[0],"Ljava/lang/Integer;")

//intersection output iterator
%{ typedef std::pair<Object,Polyhedron_3_Facet_handle_SWIG_wrapper > Object_and_Polyhedron_3_Facet_handle; %}
%define Object_and_Polyhedron_3_Facet_handle_base std::pair<CGAL::Object,Polyhedron_3_Facet_handle_SWIG_wrapper::cpp_base> %enddef
SWIG_CGAL_output_iterator_typemap_in(Primitive_iterator_helper<Polyhedron_3_Facet_handle_SWIG_wrapper_for_typemap >::output2,Object_and_Polyhedron_3_Facet_handle,Object_and_Polyhedron_3_Facet_handle,Object_and_Polyhedron_3_Facet_handle_base,SWIGTYPE_p_std__pairT_Object_SWIG_Polyhedron_3__CGAL_Facet_handleT_Polyhedron_3__t_t,"LCGAL/AABB_tree/Object_and_Polyhedron_3_Facet_handle;")
%{ typedef std::pair<Object,Polyhedron_3_Halfedge_handle_SWIG_wrapper > Object_and_Polyhedron_3_Halfedge_handle; %}
%define Object_and_Polyhedron_3_Halfedge_handle_base std::pair<CGAL::Object,Polyhedron_3_Halfedge_handle_SWIG_wrapper::cpp_base> %enddef
SWIG_CGAL_output_iterator_typemap_in(Primitive_iterator_helper<Polyhedron_3_Halfedge_handle_SWIG_wrapper_for_typemap >::output2,Object_and_Polyhedron_3_Halfedge_handle,Object_and_Polyhedron_3_Halfedge_handle,Object_and_Polyhedron_3_Halfedge_handle_base,SWIGTYPE_p_std__pairT_Object_SWIG_Polyhedron_3__CGAL_Halfedge_handleT_Polyhedron_3__t_t,"LCGAL/AABB_tree/Object_and_Polyhedron_3_Halfedge_handle;")

%{ typedef std::pair<Object,int > Object_and_Integer; %}
%define Object_and_Integer_base std::pair<CGAL::Object,int> %enddef
SWIG_CGAL_output_iterator_typemap_in(Primitive_iterator_helper< int >::output2,Object_and_Integer,Object_and_Integer,Object_and_Integer_base,SWIGTYPE_p_std__pairT_Object_int_t,"LCGAL/AABB_tree/Object_and_Integer;")
#else
%include "SWIG_CGAL/Common/Output_iterator_wrapper.h"
SWIG_CGAL_declare_generic_output_iterator(Polyhedron_3_Facet_output_iterator,Polyhedron_3_Facet_output_iterator_nested_iterator,Polyhedron_3_Facet_handle_SWIG_wrapper)
SWIG_CGAL_declare_generic_output_iterator(Polyhedron_3_Halfedge_output_iterator,Polyhedron_3_Halfedge_output_iterator_nested_iterator,Polyhedron_3_Halfedge_handle_SWIG_wrapper)
SWIG_CGAL_declare_generic_output_iterator(Integer_output_iterator,Integer_output_iterator_nested_iterator,int)
%define iObject_and_Facet std::pair<Object,Polyhedron_3_Facet_handle_SWIG_wrapper_for_typemap > %enddef
SWIG_CGAL_declare_generic_output_iterator(Polyhedron_3_Facet_and_Object_output_iterator,Polyhedron_3_Facet_and_Object_output_iterator_nested_iterator,iObject_and_Facet)
%define iObject_and_Halfedge std::pair<Object,Polyhedron_3_Halfedge_handle_SWIG_wrapper > %enddef
SWIG_CGAL_declare_generic_output_iterator(Polyhedron_3_Halfedge_and_Object_output_iterator,Polyhedron_3_Halfedge_and_Object_output_iterator_nested_iterator,iObject_and_Halfedge)
%define iObject_and_Integer std::pair<Object,int> %enddef
SWIG_CGAL_declare_generic_output_iterator(Object_and_Integer_output_iterator,Object_and_Integer_output_iterator_nested_iterator,iObject_and_Integer)
#endif

#if !SWIG_CGAL_NON_SUPPORTED_TARGET_LANGUAGE
%include "SWIG_CGAL/typemaps.i"
SWIG_CGAL_array_of_array6_of_double_to_vector_of_segment_3_typemap_in
SWIG_CGAL_array_of_array9_of_double_to_vector_of_triangle_3_typemap_in
#endif

%ignore AABB_tree_wrapper<CGAL_PTP_Tree,Polyhedron_3_Facet_handle_SWIG_wrapper,Polyhedron_3_Facet_handle_SWIG_wrapper >::insert_from_array;
%ignore AABB_tree_wrapper<CGAL_PSP_Tree,Polyhedron_3_Halfedge_handle_SWIG_wrapper,Polyhedron_3_Halfedge_handle_SWIG_wrapper >::insert_from_array;

//Declaration of the main classes
%typemap(javaimports)      AABB_tree_wrapper%{import CGAL.Polyhedron_3.Polyhedron_3_Facet_handle; import CGAL.Kernel.Triangle_3; import CGAL.Kernel.Segment_3; import CGAL.Kernel.Plane_3; import CGAL.Kernel.Ray_3; import CGAL.Kernel.Point_3; import java.util.Iterator; import java.util.Collection;%}
SWIG_CGAL_declare_identifier_of_template_class(AABB_tree_Polyhedron_3_Facet_handle,AABB_tree_wrapper<CGAL_PTP_Tree,Polyhedron_3_Facet_handle_SWIG_wrapper,Polyhedron_3_Facet_handle_SWIG_wrapper >)
%typemap(javaimports)      AABB_tree_wrapper%{import CGAL.Polyhedron_3.Polyhedron_3_Halfedge_handle; import CGAL.Kernel.Triangle_3; import CGAL.Kernel.Segment_3; import CGAL.Kernel.Plane_3; import CGAL.Kernel.Ray_3; import CGAL.Kernel.Point_3; import java.util.Iterator; import java.util.Collection;%}
SWIG_CGAL_declare_identifier_of_template_class(AABB_tree_Polyhedron_3_Halfedge_handle,AABB_tree_wrapper<CGAL_PSP_Tree,Polyhedron_3_Halfedge_handle_SWIG_wrapper,Polyhedron_3_Halfedge_handle_SWIG_wrapper >)
%typemap(javaimports)      AABB_tree_wrapper%{import CGAL.Kernel.Triangle_3; import CGAL.Kernel.Segment_3; import CGAL.Kernel.Plane_3; import CGAL.Kernel.Ray_3; import CGAL.Kernel.Point_3; import java.util.Iterator; import java.util.Collection;%}

SWIG_CGAL_declare_identifier_of_template_class(AABB_tree_Segment_3_soup,AABB_tree_wrapper<CGAL_SSP_Tree,Segment_3,int >)
SWIG_CGAL_declare_identifier_of_template_class(AABB_tree_Triangle_3_soup,AABB_tree_wrapper<CGAL_TSP_Tree,Triangle_3,int >)

#ifdef SWIG_CGAL_HAS_AABB_tree_USER_PACKAGE
%include "SWIG_CGAL/User_packages/AABB_tree/extensions.i"
#endif
