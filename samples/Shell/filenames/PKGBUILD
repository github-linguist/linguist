# Maintainer: Daniel Micay <danielmicay@gmail.com>
pkgname=stud-git
pkgver=20120316
pkgrel=1
pkgdesc="The Scalable TLS Unwrapping Daemon"
arch=(i686 x86_64)
url="https://github.com/bumptech/stud"
license=('BSD')
depends=(libev openssl)
makedepends=(git)
provides=(stud)
conflicts=(stud)

_gitroot=https://github.com/bumptech/stud.git
_gitname=stud

build() {
  cd "$srcdir"
  msg "Connecting to GIT server...."

  if [[ -d "$_gitname" ]]; then
    cd "$_gitname" && git pull origin
    msg "The local files are updated."
  else
    git clone "$_gitroot" "$_gitname"
  fi

  msg "GIT checkout done or server timeout"
  msg "Starting build..."

  rm -rf "$srcdir/$_gitname-build"
  git clone "$srcdir/$_gitname" "$srcdir/$_gitname-build"
  cd "$srcdir/$_gitname-build"

  make
}

package() {
  cd "$srcdir/$_gitname-build"
  make PREFIX=/usr DESTDIR="$pkgdir/" install
  install -Dm755 init.stud "$pkgdir/etc/rc.d/stud"
  mkdir -p "$pkgdir/etc/stud"
}
