import axios from "axios";

export default {
	async getIndex(prefix) {
		const {data} = await axios.get((prefix || "") + "/index.json");
		return data;
	},
	
	async getContent(path, prefix) {
		const {data} = await axios.get((prefix || "") + "/" + path + ".json");
		return data;
	}
}
