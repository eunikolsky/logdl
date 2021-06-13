# based on https://dev.to/cloudx/testing-our-package-build-in-the-docker-world-34p0

FROM archlinux

# binutils is for `strip` even though we're not using it
RUN pacman -Sy --noconfirm sudo fakeroot binutils

WORKDIR /build

COPY PKGBUILD .stack-work/install/x86_64-linux-*/*/*/bin/logdl-exe .

RUN useradd builduser \
    && passwd -d builduser \
    && (printf 'builduser ALL=(ALL) ALL\n' | tee -a /etc/sudoers) \
    && chown -R builduser /build \
    && sudo -u builduser bash -c 'makepkg -si --noconfirm'

CMD ["bash"]
