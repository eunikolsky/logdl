# Maintainer: Eugene Nikolsky <e@egeek.me>
pkgname=logdl
pkgver=0.3.0.0
pkgrel=1
epoch=
pkgdesc="Downloads log files from the SavySoda iOS TextEditor"
arch=('x86_64')
url="https://alice.egeek.me/"
license=('MIT')
groups=()
depends=('glibc' 'gmp')
makedepends=()
checkdepends=()
optdepends=()
provides=()
conflicts=()
replaces=()
backup=()
options=()
install=
changelog=
source=(logdl-exe)
noextract=()
md5sums=('SKIP')
validpgpkeys=()
strip=()

package() {
  install -Dm 755 -t "$pkgdir"/usr/bin logdl-exe
}
