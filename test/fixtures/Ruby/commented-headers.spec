# Name: metrics-agent
# Version: 2.4
# Release: 1
# %description

require_relative "spec_helper"

describe PackageMetadata do
  it "loads the package version" do
    expect(PackageMetadata.version).to eq("2.4")
  end
end
