
terra mything

terra mybar()
	return mything(4)
end

terra mything(a : int)
	return a
end

terra mything

terra mybar2()
	return mything(4)
end

assert(mybar() == mybar2())