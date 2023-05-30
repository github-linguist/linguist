// file      : libbuild2/target.txx -*- C++ -*-
// license   : MIT; see accompanying LICENSE file

#include <libbuild2/scope.hxx>
#include <libbuild2/diagnostics.hxx>

namespace build2
{
  template <const char* ext>
  const char*
  target_extension_fix (const target_key& tk, const scope*)
  {
    // A generic file target type doesn't imply any extension while a very
    // specific one (say man1) may have a fixed extension. So if one wasn't
    // specified set it to fixed ext rather than unspecified. For file{}
    // itself we make it empty which means we treat file{foo} as file{foo.}.
    //
    return tk.ext ? tk.ext->c_str () : ext;
  }

  template <const char* ext>
  bool
  target_pattern_fix (const target_type&,
                      const scope&,
                      string& v,
                      optional<string>& e,
                      const location& l,
                      bool r)
  {
    if (r)
    {
      // If we get called to reverse then it means we've added the extension
      // in the first place.
      //
      assert (e);
      e = nullopt;
    }
    else
    {
      e = target::split_name (v, l);

      // We only add our extension if there isn't one already.
      //
      if (!e)
      {
        e = ext;
        return true;
      }
    }

    return false;
  }

  inline optional<string>
  target_extension_var_impl (const target_type& tt,
                             const string& tn,
                             const scope& s,
                             const char* def)
  {
    // Include target type/pattern-specific variables.
    //
    // Note that we know this is not dir{} or fsdir{} and that the extension
    // is not part of the match (see variable_type_map::find() for details).
    //
    if (auto l = s.lookup (*s.ctx.var_extension, tt, tn))
    {
      // Help the user here and strip leading '.' from the extension.
      //
      const string& e (cast<string> (l));
      return !e.empty () && e.front () == '.' ? string (e, 1) : e;
    }

    return def != nullptr ? optional<string> (def) : nullopt;
  }

  template <const char* def>
  optional<string>
  target_extension_var (const target_key& tk,
                        const scope& s,
                        const char*,
                        bool)
  {
    return target_extension_var_impl (*tk.type, *tk.name, s, def);
  }

  template <const char* def>
  bool
  target_pattern_var (const target_type& tt,
                      const scope& s,
                      string& v,
                      optional<string>& e,
                      const location& l,
                      bool r)
  {
    if (r)
    {
      // If we get called to reverse then it means we've added the extension
      // in the first place.
      //
      assert (e);
      e = nullopt;
    }
    else
    {
      e = target::split_name (v, l);

      // We only add our extension if there isn't one already.
      //
      if (!e)
      {
        // Use empty name as a target since we only want target type/pattern-
        // specific variables that match any target ('*' but not '*.txt').
        //
        if ((e = target_extension_var_impl (tt, string (), s, def)))
          return true;
      }
    }

    return false;
  }

  // dir
  //
  template <typename K>
  const target* dir::
  search_implied (const scope& bs, const K& k, tracer& trace)
  {
    using namespace butl;

    // See if we have any prerequisites.
    //
    prerequisites_type ps (collect_implied (bs));

    if (ps.empty ())
      return nullptr;

    l5 ([&]{trace << "implying buildfile for " << k;});

    // We behave as if this target was explicitly mentioned in the (implied)
    // buildfile. Thus not implied.
    //
    target& t (bs.ctx.targets.insert (dir::static_type,
                                      bs.out_path (),
                                      dir_path (),
                                      string (),
                                      nullopt,
                                      target_decl::real,
                                      trace).first);
    t.prerequisites (move (ps));
    return &t;
  }

  // exe
  //
  template <typename T>
  const T* exe::
  lookup_metadata (const char* var) const
  {
    if (auto* ns = cast_null<names> (vars[ctx.var_export_metadata]))
    {
      // Metadata variable prefix must be in the second name.
      //
      if (ns->size () < 2 || !(*ns)[1].simple ())
        fail << "invalid metadata variable prefix in target " << *this;

      return cast_null<T> (vars[(*ns)[1].value + '.' + var]);
    }

    return nullptr;
  }
}
