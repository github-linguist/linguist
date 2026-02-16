# frozen_string_literal: true

[
  Licensee::Projects::FSProject,
  Licensee::Projects::GitProject,
  Licensee::Projects::GitHubProject
].each do |project_type|
  RSpec.describe project_type do
    subject { described_class.new(path) }

    let(:stubbed_org) { '_licensee_test_fixture' }
    let(:mit) { Licensee::License.find('mit') }
    let(:other) { Licensee::License.find('other') }
    let(:fixture) { 'mit' }
    let(:path) { fixture_path(fixture) }
    let(:api_base) { 'https://api.github.com/repos' }

    if described_class == Licensee::Projects::GitProject
      before do
        Dir.chdir path do
          `git init`
          `git config --local commit.gpgsign false`
          `git add .`
          `git commit -m 'initial commit'`
        end
      end

      after do
        subject.close
        FileUtils.rm_rf File.expand_path '.git', path
      end
    elsif described_class == Licensee::Projects::GitHubProject
      before do
        stub_request(
          :get, "#{api_base}/#{stubbed_org}/#{fixture}/contents/"
        ).to_return(
          status:  200,
          body:    fixture_root_contents_from_api(fixture),
          headers: { 'Content-Type' => 'application/json' }
        )

        fixture_root_files(fixture).each do |file|
          relative_path = File.basename(file)
          parts = [api_base, stubbed_org, fixture, 'contents', relative_path]
          stub_request(:get, parts.join('/'))
            .with(headers: { 'accept' => 'application/vnd.github.v3.raw' })
            .to_return(status: 200, body: File.read(file))
        end
      end

      let(:path) { "https://github.com/#{stubbed_org}/#{fixture}" }
    end

    if described_class == Licensee::Projects::GitProject
      context 'when initialized with a repo' do
        subject { described_class.new(repo) }

        let(:repo) { Rugged::Repository.new(path) }

        it 'returns the repository' do
          expect(subject.repository).to be_a(Rugged::Repository)
        end
      end

      context 'when initialized with a revision' do
        let(:revision) { subject.repository.last_commit.oid }

        before do
          subject.instance_variable_set(:@revision, revision)
        end

        it 'returns the commit' do
          expect(subject.send(:commit).oid).to eql(revision)
        end
      end
    end

    it 'returns the license' do
      expect(subject.license).to be_a(Licensee::License)
      expect(subject.license).to eql(mit)
    end

    it 'returns the matched file' do
      expect(subject.matched_file).to be_a(Licensee::ProjectFiles::LicenseFile)
      expect(subject.matched_file.filename).to eql('LICENSE.txt')
    end

    it 'returns the license file' do
      expect(subject.license_file).to be_a(Licensee::ProjectFiles::LicenseFile)
      expect(subject.license_file.filename).to eql('LICENSE.txt')
    end

    it "doesn't return the readme" do
      expect(subject.readme_file).to be_nil
    end

    it "doesn't return the package file" do
      expect(subject.package_file).to be_nil
    end

    context 'reading files' do
      let(:files) { subject.send(:files) }

      it 'returns the file list' do
        expect(files.count).to be(2)
        license = files.find { |f| f[:name] == 'LICENSE.txt' }
        expect(license).not_to be_nil

        expect(files.first).to have_key(:oid) if described_class == Licensee::Projects::GitProject
      end

      it "returns a file's content" do
        content = subject.send(:load_file, files.first)
        expect(content).to match('Permission is hereby granted')
      end

      if described_class == Licensee::Projects::FSProject
        context 'with search root argument' do
          subject { described_class.new(path, search_root: search_root) }

          let(:fixture) { 'license-in-parent-folder/license-folder/package' }
          let(:path) { fixture_path(fixture) }
          let(:license_folder) { 'license-in-parent-folder/license-folder' }
          let(:search_root) { fixture_path(license_folder) }
          let(:files) { subject.send(:files) }

          it 'looks for licenses in parent directories up to the search root' do
            # should not include the license in 'license-in-parent-folder' dir
            expect(files.count).to be(1)
            expect(files.first[:name]).to eql('LICENSE.txt')
          end
        end

        context 'without search root argument' do
          let(:fixture) { 'license-in-parent-folder/license-folder/package' }

          it 'looks for licenses in current directory only' do
            expect(files.count).to be(0)
          end
        end
      end
    end

    context 'encoding correctness' do
      let(:fixture) { 'copyright-encoding' }

      it "returns a file's content" do
        expect(subject.license_file.content).to match(
          'Copyright © 2013–2016 by Peder Ås, 王二麻子, and Seán Ó Rudaí'
        )
      end
    end

    context 'readme detection' do
      subject { described_class.new(path, detect_readme: true) }

      let(:fixture) { 'readme' }

      it 'returns the readme' do
        expect(subject.readme_file).to be_a(Licensee::ProjectFiles::ReadmeFile)
        expect(subject.readme_file.filename).to eql('README.md')
      end

      it 'returns the license' do
        expect(subject.license).to be_a(Licensee::License)
        expect(subject.license).to eql(mit)
      end
    end

    context 'package manager detection' do
      subject { described_class.new(path, detect_packages: true) }

      let(:fixture) { 'gemspec' }

      # Using a `.gemspec` extension in the fixture breaks `gem release`
      before do
        from = "#{fixture_path(fixture)}/project._gemspec"
        to   = "#{fixture_path(fixture)}/project.gemspec"
        FileUtils.cp(from, to)
        if described_class == Licensee::Projects::GitProject
          Dir.chdir fixture_path(fixture) do
            `git add project.gemspec`
            `git commit -m 'add real gemspec'`
          end
        end
      end

      if described_class == Licensee::Projects::GitHubProject
        before do
          stub_request(
            :get, "#{api_base}/#{stubbed_org}/#{fixture}/contents/"
          ).to_return(
            status:  200,
            body:    fixture_root_contents_from_api(fixture),
            headers: { 'Content-Type' => 'application/json' }
          )

          file = fixture_path "#{fixture}/project.gemspec"
          relative_path = File.basename(file)
          parts = [api_base, stubbed_org, fixture, 'contents', relative_path]
          stub_request(:get, parts.join('/'))
            .with(headers: { 'accept' => 'application/vnd.github.v3.raw' })
            .to_return(status: 200, body: File.read(file))
        end
      end

      after do
        FileUtils.rm("#{fixture_path(fixture)}/project.gemspec")
      end

      it 'returns the package file' do
        expected = Licensee::ProjectFiles::PackageManagerFile
        expect(subject.package_file).to be_a(expected)
        expect(subject.package_file.filename).to eql('project.gemspec')
      end

      it 'returns the license' do
        expect(subject.license).to be_a(Licensee::License)
        expect(subject.license).to eql(mit)
      end
    end

    context 'multiple licenses' do
      let(:fixture) { 'multiple-license-files' }

      it 'returns other for license' do
        expect(subject.license).to eql(other)
      end

      it 'returns nil for matched_file' do
        expect(subject.matched_file).to be_nil
      end

      it 'returns nil for license_file' do
        expect(subject.license_file).to be_nil
      end

      it 'returns both licenses' do
        expect(subject.licenses.count).to be(2)
        expect(subject.licenses.first).to eql(Licensee::License.find('mpl-2.0'))
        expect(subject.licenses.last).to eql(mit)
      end

      it 'returns both matched_files' do
        expect(subject.matched_files.count).to be(2)
        expect(subject.matched_files.first.filename).to eql('LICENSE')
        expect(subject.matched_files.last.filename).to eql('LICENSE.txt')
      end

      it 'returns both license_files' do
        expect(subject.license_files.count).to be(2)
        expect(subject.license_files.first.filename).to eql('LICENSE')
        expect(subject.license_files.last.filename).to eql('LICENSE.txt')
      end
    end

    context 'lgpl' do
      let(:gpl) { Licensee::License.find('gpl-3.0') }
      let(:lgpl) { Licensee::License.find('lgpl-3.0') }
      let(:fixture) { 'lgpl' }

      it 'license returns lgpl' do
        expect(subject.license).to eql(lgpl)
      end

      it 'matched_file returns copying.lesser' do
        expect(subject.matched_file).not_to be_nil
        expect(subject.matched_file.filename).to eql('COPYING.lesser')
      end

      it 'license_file returns copying.lesser' do
        expect(subject.license_file).not_to be_nil
        expect(subject.license_file.filename).to eql('COPYING.lesser')
      end

      it 'returns both licenses' do
        expect(subject.licenses.count).to be(2)
        expect(subject.licenses.first).to eql(lgpl)
        expect(subject.licenses.last).to eql(gpl)
      end

      it 'returns both matched_files' do
        expect(subject.matched_files.count).to be(2)
        expect(subject.matched_files.first.filename).to eql('COPYING.lesser')
        expect(subject.matched_files.last.filename).to eql('LICENSE')
      end

      it 'returns both license_files' do
        expect(subject.license_files.count).to be(2)
        expect(subject.license_files.first.filename).to eql('COPYING.lesser')
        expect(subject.license_files.last.filename).to eql('LICENSE')
      end
    end

    context 'with a copyright file' do
      let(:fixture) { 'mit-with-copyright' }

      it 'returns MIT' do
        expect(subject.license).to eql(mit)
      end
    end

    context 'to_h' do
      let(:hash) { subject.to_h }
      let(:expected) do
        {
          licenses:      subject.licenses.map(&:to_h),
          matched_files: subject.matched_files.map(&:to_h)
        }
      end

      it 'Converts to a hash' do
        expect(hash).to eql(expected)
      end
    end
  end
end
