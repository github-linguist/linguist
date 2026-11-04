export function DocsFooter() @{
	<>
		<footer class="docs-footer">
			<div class="footer-content">
				<p class="footer-message">Released under the MIT License.</p>
				<p class="footer-copyright">Copyright © 2025-present Dominic Gannaway</p>
			</div>
		</footer>
		<style>
			.docs-footer {
				border-top: 1px solid #2e2e32;
				padding: 32px 0;
				margin-top: 32px;
			}

			.footer-content {
				text-align: center;
			}

			.footer-message,
			.footer-copyright {
				font-size: 14px;
				font-weight: 500;
				color: rgba(235, 235, 245, 0.6);
				margin: 0;
				line-height: 24px;
			}
		</style>
	</>
}
