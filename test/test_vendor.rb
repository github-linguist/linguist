require 'linguist/file_blob'
require 'linguist/vendor'
require 'linguist/samples'

require 'test/unit'

class TestVendor < Test::Unit::TestCase
  include Linguist

  def samples_path
    File.expand_path("../../samples", __FILE__)
  end

  def vendor
    @vendor ||= Vendor.new Dir["#{samples_path}/**/*"].
      select { |f| File.file?(f) }.
      map { |path| FileBlob.new(path, samples_path) }
  end

  def blob(name)
    name = File.join(samples_path, name) unless name =~ /^\//
    FileBlob.new(name, samples_path)
  end

  def assert_vendored(name)
    assert vendor.vendored?(blob(name)), "Expected '#{name}' to be vendored, but it wasn't"
  end

  def assert_not_vendored(name)
    assert !vendor.vendored?(blob(name)), "Expected '#{name}' to not be vendored, but it was"
  end

  def test_globally_vendored
    assert_not_vendored "Text/README"
    assert_not_vendored "ext/extconf.rb"

    # Node depedencies
    assert_vendored "node_modules/coffee-script/lib/coffee-script.js"

    # Rails vendor/
    assert_vendored "vendor/plugins/will_paginate/lib/will_paginate.rb"

    # C deps
    assert_vendored "deps/http_parser/http_parser.c"
    assert_vendored "deps/v8/src/v8.h"

    # Prototype
    assert_not_vendored "public/javascripts/application.js"
    assert_vendored "public/javascripts/prototype.js"
    assert_vendored "public/javascripts/effects.js"
    assert_vendored "public/javascripts/controls.js"
    assert_vendored "public/javascripts/dragdrop.js"

    # jQuery
    assert_vendored "jquery.js"
    assert_vendored "public/javascripts/jquery.js"
    assert_vendored "public/javascripts/jquery.min.js"
    assert_vendored "public/javascripts/jquery-1.7.js"
    assert_vendored "public/javascripts/jquery-1.7.min.js"
    assert_vendored "public/javascripts/jquery-1.5.2.js"
    assert_vendored "public/javascripts/jquery-1.6.1.js"
    assert_vendored "public/javascripts/jquery-1.6.1.min.js"
    assert_not_vendored "public/javascripts/jquery.github.menu.js"

    # MooTools
    assert_vendored "public/javascripts/mootools-core-1.3.2-full-compat.js"
    assert_vendored "public/javascripts/mootools-core-1.3.2-full-compat-yc.js"

    # Dojo
    assert_vendored "public/javascripts/dojo.js"

    # MochiKit
    assert_vendored "public/javascripts/MochiKit.js"

    # YUI
    assert_vendored "public/javascripts/yahoo-dom-event.js"
    assert_vendored "public/javascripts/yahoo-min.js"
    assert_vendored "public/javascripts/yuiloader-dom-event.js"

    # LESS
    assert_vendored "public/javascripts/less-1.1.0.js"
    assert_vendored "public/javascripts/less-1.1.0.min.js"

    # WYS editors
    assert_vendored "public/javascripts/ckeditor.js"
    assert_vendored "public/javascripts/tiny_mce.js"
    assert_vendored "public/javascripts/tiny_mce_popup.js"
    assert_vendored "public/javascripts/tiny_mce_src.js"

    # Fabric
    assert_vendored "fabfile.py"

    # WAF
    assert_vendored "waf"

    # Visual Studio IntelliSense
    assert_vendored "Scripts/jquery-1.7-vsdoc.js"

    # Microsoft Ajax
    assert_vendored "Scripts/MicrosoftAjax.debug.js"
    assert_vendored "Scripts/MicrosoftAjax.js"
    assert_vendored "Scripts/MicrosoftMvcAjax.debug.js"
    assert_vendored "Scripts/MicrosoftMvcAjax.js"
    assert_vendored "Scripts/MicrosoftMvcValidation.debug.js"
    assert_vendored "Scripts/MicrosoftMvcValidation.js"

    # jQuery validation plugin (MS bundles this with asp.net mvc)
    assert_vendored "Scripts/jquery.validate.js"

    # NuGet Packages
    assert_vendored "packages/Modernizr.2.0.6/Content/Scripts/modernizr-2.0.6-development-only.js"
  end

  def test_vendored_per_repo
    assert_vendored "VimL/ignored.vim"
  end
end
