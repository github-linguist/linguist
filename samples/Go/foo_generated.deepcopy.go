package v1

type Example struct{}

func (in *Example) DeepCopy() *Example {
	if in == nil {
		return nil
	}
	out := new(Example)
	*out = *in
	return out
}
