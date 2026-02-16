# frozen_string_literal: true

RSpec.describe Licensee::Projects::GitHubProject do
  subject(:instance) { described_class.new(github_url) }

  let(:repo) { 'benbalter/licensee' }
  let(:github_url) { "https://github.com/#{repo}" }
  let(:mit) { Licensee::License.find('mit') }
  let(:mit_readme_file) { File.read(fixture_path('mit/README.md')) }
  let(:mit_license_file) { File.read(fixture_path('mit/LICENSE.txt')) }
  let(:apache2) { Licensee::License.find('apache-2.0') }
  let(:apache2_license_file) { File.read(fixture_path('apache-with-readme-notice/LICENSE')) }

  describe '#initialize' do
    context 'with a GitHub URI' do
      it 'sets @repo' do
        expect(instance.repo).to eq(repo)
      end
    end

    context 'with a GitHub git URI' do
      let(:github_url) { "https://github.com/#{repo}.git" }

      it 'sets @repo, stripping the trailing extension' do
        expect(instance.repo).to eq(repo)
      end
    end

    context 'with a non-GitHub URI' do
      let(:github_url) { "https://gitlab.com/#{repo}" }

      it 'raises an ArgumentError' do
        expect { instance }.to raise_error(ArgumentError)
      end
    end

    context 'with a local folder' do
      let(:github_url) { fixture_path('mit') }

      it 'raises an ArgumentError' do
        expect { instance }.to raise_error(ArgumentError)
      end
    end
  end

  context 'when the repo exists' do
    before do
      stub_request(:get, 'https://api.github.com/repos/benbalter/licensee/contents/')
        .to_return(
          status:  200,
          body:    fixture_contents('webmock/licensee.json'),
          headers: { 'Content-Type' => 'application/json' }
        )

      stub_request(:get, 'https://api.github.com/repos/benbalter/licensee/contents/LICENSE.txt')
        .with(headers: { 'accept' => 'application/vnd.github.v3.raw' })
        .to_return(status: 200, body: mit_license_file)

      stub_request(:get, 'https://api.github.com/repos/benbalter/licensee/contents/README.md')
        .with(headers: { 'accept' => 'application/vnd.github.v3.raw' })
        .to_return(status: 200, body: mit_readme_file)
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

    context 'readme detection' do
      subject { described_class.new(github_url, detect_readme: true) }

      it 'returns the readme' do
        expect(subject.readme_file).to be_a(Licensee::ProjectFiles::ReadmeFile)
        expect(subject.readme_file.filename).to eql('README.md')
      end

      it 'returns the license' do
        expect(subject.license).to be_a(Licensee::License)
        expect(subject.license).to eql(mit)
      end
    end

    context 'when initialized with a ref' do
      subject { described_class.new(github_url, ref: 'my-ref') }

      before do
        stub_request(:get, 'https://api.github.com/repos/benbalter/licensee/contents/?ref=my-ref')
          .to_return(
            status:  200,
            body:    fixture_contents('webmock/licensee_alternate_ref.json'),
            headers: { 'Content-Type' => 'application/json' }
          )

        stub_request(:get, 'https://api.github.com/repos/benbalter/licensee/contents/LICENSE?ref=my-ref')
          .with(headers: { 'accept' => 'application/vnd.github.v3.raw' })
          .to_return(status: 200, body: apache2_license_file)
      end

      it 'returns the ref' do
        expect(subject.ref).to eql('my-ref')
      end

      it 'returns query params' do
        expect(subject.send(:query_params)).to eql({ ref: subject.ref })
      end

      it 'returns the license' do
        expect(subject.license).to be_a(Licensee::License)
        expect(subject.license).to eql(apache2)
      end
    end
  end

  context 'when the repo cannot be found' do
    let(:repo) { 'benbalter/not-foundsss' }

    before do
      stub_request(:get, 'https://api.github.com/repos/benbalter/not-foundsss/contents/')
        .to_return(status: 404)
    end

    it 'raises a RepoNotFound error' do
      expect { subject.license }.to raise_error(described_class::RepoNotFound)
    end
  end
end
