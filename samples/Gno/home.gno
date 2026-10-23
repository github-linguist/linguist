package home

import (
	"chain/runtime"
	"strconv"

	"gno.land/p/leon/svgbtn"
	"gno.land/p/moul/dynreplacer"
	"gno.land/p/nt/ownable/v0"
	"gno.land/r/devrels/events"
	blog "gno.land/r/gnoland/blog"
)

var (
	override string
	Admin    = ownable.NewWithAddress("g1rp7cmetn27eqlpjpc4vuusf8kaj746tysc0qgh") // govdao t1 multisig
)

func Render(_ string) string {
	r := dynreplacer.New()
	r.RegisterCallback(":latest-blogposts:", func() string {
		return blog.RenderLastPostsWidget(4)
	})
	r.RegisterCallback(":upcoming-events:", func() string {
		out, _ := events.RenderEventWidget(events.MaxWidgetSize)
		return out
	})
	r.RegisterCallback(":qotb:", quoteOfTheBlock)
	r.RegisterCallback(":newsletter-button:", newsletterButton)
	r.RegisterCallback(":chain-height:", func() string {
		return strconv.Itoa(int(runtime.ChainHeight()))
	})

	template := `# Welcome to Gno.land

We're building Gno.land, set to become the leading open-source smart contract
platform, using Gno, an interpreted and fully deterministic variation of the
Go programming language for succinct and composable smart contracts.

With transparent and timeless code, Gno.land is the next generation of smart
contract platforms, serving as the "GitHub" of the ecosystem, with realms built
using fully transparent, auditable code that anyone can inspect and reuse.

Intuitive and easy to use, Gno.land lowers the barrier to web3 and makes
censorship-resistant platforms accessible to everyone. If you want to help lay
the foundations of a fairer and freer world, join us today.

---

## [Boards](/r/gnoland/boards2/v1) - On-chain forum for the Gno.land community

**Post, discuss, and create your content community**: Boards is a fully on-chain social forum to create Boards topics, post threads, comment and reply. A plug-and-deploy DAO lets communities manage content, permissions and moderation their way.

Explore this ready-to-use Gno dApp, and experience decentralized social media in action.

**[Open Boards](/r/gnoland/boards2/v1)**

---

<gno-columns>
## Learn about Gno.land

- [About](/about)
- [GitHub](https://github.com/gnolang)
- [Blog](/blog)
- [Events](/events)
- [Partners, Fund, Grants](/partners)
- [Explore the Ecosystem](/ecosystem)
- [Careers](https://jobs.ashbyhq.com/allinbits)

<gno-columns-sep>

## Build with Gno

- [Write Gno in the browser](https://play.gno.land)
- [Read about the Gno Language](/gnolang)
- [Visit the official documentation](https://docs.gno.land)
- [Efficient local development for Gno](https://docs.gno.land/resources/gnodev)
- [Get testnet GNOTs](https://faucet.gno.land)

<gno-columns-sep>

## Explore the universe

- [Discover demo packages](https://github.com/gnolang/gno/tree/master/examples)
- [Gnoscan](https://gnoscan.io)
- [Gno networks documentation](https://docs.gno.land/resources/gnoland-networks/)
- [Staging](https://staging.gno.land/)
- [Testnet 12](https://test12.testnets.gno.land/)
- [Faucet Hub](https://faucet.gno.land)

</gno-columns>

<gno-columns>

## [Latest Blogposts](/r/gnoland/blog)

:latest-blogposts:

<gno-columns-sep>

## [Latest Events](/events)

:upcoming-events:

</gno-columns>

---

## [Gno Playground](https://play.gno.land)

Gno Playground is a web application designed for building, running, testing, and
interacting with your Gno code, enhancing your understanding of the Gno
language. With Gno Playground, you can share your code, execute tests, deploy
your realms and packages to Gno.land, and explore a multitude of other features.

Experience the convenience of code sharing and rapid experimentation with
[Gno Playground](https://play.gno.land).

---

## Explore New Packages and Realms

All code in Gno.land is organized in packages, and each package lives at a unique package path like
"r/gnoland/home". You can browse packages, inspect their source, and use them in your own libraries and realms.

<gno-columns>

### r/gnoland

Official realm packages developed by the Gno.land core team.

[Browse](/r/gnoland)

<gno-columns-sep>

### r/sys

System-level realm packages used by the chain.

[Browse](/r/sys)

<gno-columns-sep>

### r/demo

Demo realm packages showcasing what’s possible.

[Browse](/r/demo)

<gno-columns-sep>

### p/demo

Pure packages for demo purposes.

[Browse](/p/demo)

</gno-columns>

---

<gno-columns>

## Socials

- Check out our [community projects](https://github.com/gnolang/awesome-gno)
- [Discord](https://discord.gg/S8nKUqwkPn)
- [Twitter](https://twitter.com/_gnoland)
- [Youtube](https://www.youtube.com/@_gnoland)
- [Telegram](https://t.me/gnoland)

<gno-columns-sep>

## Quote of the ~Day~ Block #:chain-height:

> :qotb:

</gno-columns>

---

## Sign up for our newsletter

Stay in the Gno by signing up for our newsletter. You'll get the scoop on dev updates, fresh content, and community news.

:newsletter-button:

---

**This is a testnet.** Package names are not guaranteed to be available for production.`

	if override != "" {
		template = override
	}
	result := r.Replace(template)
	return result
}

func newsletterButton() string {
	return svgbtn.Button(
		256,
		44,
		"#226c57",
		"#ffffff",
		"Subscribe to stay in the Gno",
		"https://land.us18.list-manage.com/subscribe?u=8befe3303cf82796d2c1a1aff&id=271812000b",
	)
}

func quoteOfTheBlock() string {
	quotes := []string{
		"Gno is for Truth.",
		"Gno is for Social Coordination.",
		"Gno is _not only_ for DeFi.",
		"Now, you Gno.",
		"Come for the Go, Stay for the Gno.",
	}
	height := runtime.ChainHeight()
	idx := int(height) % len(quotes)
	qotb := quotes[idx]
	return qotb
}

func AdminSetOverride(cur realm, content string) {
	Admin.AssertOwnedBy(cur.Previous().Address())
	override = content
}

func AdminTransferOwnership(cur realm, newOwner address) {
	if err := Admin.TransferOwnership(0, cur, newOwner); err != nil {
		panic(err)
	}
}
