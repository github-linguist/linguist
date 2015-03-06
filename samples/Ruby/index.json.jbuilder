json.array!(@courts) do |court|
  json.extract! court, :id, :name_r, :region, :region_r, :email, :website
  json.url court_url(court, format: :json)
end
