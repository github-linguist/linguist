import config from "../webpack.config";
import webpackDevMiddleware from "webpack-dev-middleware";
import webpackHot from "webpack-hot-middleware";
import webpack from "webpack";
import express from "express";

app.use(webpackDevMiddleware(compiler, {
	noInfo: false,
	quiet: false,
	publicPath: config.output.publicPath,
	hot: true,
	historyApiFallback: true
}));
	
app.get("/(:root).json", (req, resp) => {
	resp.send(indexer.index(req.params.root));
});

export default function(){
	const server = http.createServer(app);
	
	server.listen(3000);
	
	const wss = new WebSocketServer({server});
	
	let id = 1;
	wss.on("connection", (ws) => {
		console.log("Hello", " world");
		let wsId = id++;
		sessions[wsId] = ws;
		ws.on("close", () => {
			delete sessions[wsId]
		});
	});
};
