;;; SPDX-FileCopyrightText: © 2024 Jean-Pierre De Jesus DIAZ <me@jeandudey.tech>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (raspberry packages firmware)
  #:use-module (gnu artwork)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages assembly)
  #:use-module (gnu packages cross-base)
  #:use-module (gnu packages firmware)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages inkscape)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages python)
  #:use-module (guix build-system trivial)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils))

(define make-arm-trusted-firmware
  (@@ (gnu packages firmware) make-arm-trusted-firmware))

(define-public arm-trusted-firmware-rpi4
  (let ((base (make-arm-trusted-firmware "rpi4")))
    (package
      (inherit base)
      (arguments
       (substitute-keyword-arguments (package-arguments base)
         ((#:make-flags make-flags)
          #~(append (list "RPI3_PRELOADED_DTB_BASE=0x1F0000"
                          "PRELOADED_BL33_BASE=0x20000"
                          "SUPPORT_VFP=1"
                          "SMC_PCI_SUPPORT=1")
                    #$make-flags)))))))


;; The EDK2 source package necessary to build the image.
;;
;; Only uses the Raspberry Pi platform code and removes the
;; TrustedFirmware binary blob and replaces it with our build
;; and also we replace the Raspberry Pi logo as it is trademarked.
;;
;; While the trademark rules in the Raspberry Pi website are very clear
;; and this package wouldn't technically be in breach it is better
;; just to replace it with another to avoid any issues altogether.
;;
;; Better be safe than sorry.
(define-public edk2-rpi4-osi
  (let ((revision "0")
        (commit "ea2040c2d4e2200557e87b9f9fbd4f8fb7a2b6e8"))
    (package
      (name "edk2-rpi4-osi")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                       (url "https://github.com/tianocore/edk2-non-osi")
                       (commit commit)))
                (file-name (git-file-name "edk2-non-osi" version))
                (modules '((guix build utils)))
                (snippet
                  #~(for-each delete-file-recursively
                              '("Drivers"
                                "Emulator"
                                "Platform/AMD"
                                "Platform/ARM"
                                "Platform/Hisilicon"
                                "Platform/Intel"
                                "Platform/LeMaker"
                                "Platform/Qemu/Sbsa"
                                "Platform/RaspberryPi/Drivers/LogoDxe/License.txt"
                                "Platform/RaspberryPi/Drivers/LogoDxe/Logo.bmp"
                                "Platform/RaspberryPi/Drivers/LogoDxe/Logo.eps"
                                "Platform/RaspberryPi/RPi3"
                                "Platform/RaspberryPi/RPi4"
                                "Platform/Socionext"
                                "Platform/SoftIron"
                                "Silicon")))
                (sha256
                 (base32
                  "0yw18raqhyin5rak9maa5znwqnyzaqqsp22g78865xmj06akh1l1"))))
      (build-system trivial-build-system)
      (arguments
        (list #:modules '((guix build utils))
              #:builder
              #~(begin
                  (use-modules (guix build utils))

                  (let* ((dxe-dir "Platform/RaspberryPi/Drivers/LogoDxe")
                         (in-dxe-dir (string-append (assoc-ref %build-inputs "source")
                                                    "/" dxe-dir))
                         (out-dxe-dir (string-append #$output
                                                     "/src/edk2-rpi4-osi/"
                                                     dxe-dir)))
                    (mkdir-p out-dxe-dir)
                    (copy-recursively in-dxe-dir out-dxe-dir)

                    (mkdir-p (string-append
                               #$output "/src/edk2-rpi4-osi/Platform"
                               "/RaspberryPi/RPi4/TrustedFirmware"))
                    (copy-file #$(file-append
                                   (this-package-input "arm-trusted-firmware-rpi4")
                                   "/bl31.bin")
                               (string-append
                                 #$output "/src/edk2-rpi4-osi/Platform"
                                 "/RaspberryPi/RPi4/TrustedFirmware/bl31.bin"))

                    ;; Convert SVG to PNG.
                    (invoke #$(file-append
                                (this-package-native-input "inkscape")
                                "/bin/inkscape")
                            #$(file-append %artwork-repository
                                           "/logo/Guix.svg")
                            "--export-background=black"
                            "-o" (string-append out-dxe-dir "/Logo.png"))

                    ;; Convert to BMP.
                    (invoke #$(file-append
                                (this-package-native-input "imagemagick")
                                "/bin/convert")
                            (string-append out-dxe-dir "/Logo.png")
                            (string-append "BMP3:" out-dxe-dir "/Logo.bmp"))))))
      (native-inputs (list imagemagick inkscape))
      (inputs (list arm-trusted-firmware-rpi4))
      (home-page "https://github.com/tianocore/edk2-non-osi")
      (synopsis "Logo driver sources for Raspberry Pi 4 EDK2")
      (description "This package provides modified sources for displaying a
logo in the Raspberry Pi 4 EDK2 UEFI firmware.  It is based on the non-OSI
sources of EDK2 but removing all of the binary blobs and trademarked logos
so that it is safe for usage and redistribution.")
      (license license:bsd-2))))

(define-public edk2-rpi4
  (let ((toolchain "GCC")
        (arch-string "AARCH64"))
    (package
      (inherit edk2-tools)
      (name "edk2-rpi4")
      (version "202502")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/tianocore/edk2")
                      (commit (string-append "edk2-stable" version))
                      ;; EDK2 makes extensive use of submodules.
                      (recursive? #t)))
                (file-name (git-file-name "edk2-tools" version))
                (sha256
                 (base32
                  "026h7hadzj1zflgf4qzdby3gmgcqh3m5rvn2yr92jjwn4z8c51la"))))
      (arguments
       (list
        #:tests? #f                     ; No check target.
        #:target #f                     ; Package produces firmware.
        #:modules '((guix build gnu-build-system)
                    (guix build utils)
                    (ice-9 match))
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'patch-source
              (lambda _
                (substitute* "edksetup.sh"
                  (("^return \\$\\?")
                   "exit $?"))))
            (add-before 'configure 'set-env
              (lambda _
                (unless (string-prefix? "aarch64" #$(%current-system))
                  (setenv (string-append #$toolchain "_AARCH64_PREFIX")
                          "aarch64-linux-gnu-"))))
            (replace 'configure
              (lambda _
                (let* ((cwd (getcwd))
                       (tools (string-append cwd "/BaseTools"))
                       (bin (string-append tools "/BinWrappers/PosixLike")))
                  (setenv "WORKSPACE" cwd)
                  (setenv "EDK_TOOLS_PATH" tools)
                  (setenv "PYTHON3_ENABLE" "TRUE")
                  (setenv "PYTHON_COMMAND" "python3")
                  (setenv "PATH" (string-append (getenv "PATH") ":" bin))
                  (setenv "PACKAGES_PATH"
                          (string-append
                            cwd ":"
                            #$(this-package-native-input "edk2-platforms") ":"
                            #$(this-package-native-input "edk2-rpi4-osi")
                            "/src/edk2-rpi4-osi"))
                  (invoke "bash" "edksetup.sh")
                  (substitute* "Conf/target.txt"
                    (("^TARGET[ ]*=.*$") "TARGET = RELEASE\n")
                    (("^TOOL_CHAIN_TAG[ ]*=.*$")
                     (string-append "TOOL_CHAIN_TAG = " #$toolchain "\n"))
                    (("^TARGET_ARCH[ ]*=.*$")
                     (string-append "TARGET_ARCH = " #$arch-string
                                    "\n"))
                    (("^MAX_CONCURRENT_THREAD_NUMBER[ ]*=.*$")
                     (format #f "MAX_CONCURRENT_THREAD_NUMBER = ~a~%"
                             (number->string (parallel-job-count)))))
                  ;; Build build support.
                  (setenv "CC" "gcc")
                  (invoke "make" "-C" tools))))
            (replace 'build
              (lambda _
                (invoke "build" "-a" #$arch-string "-t" #$toolchain "-p"
                        (string-append
                          #$(this-package-native-input "edk2-platforms")
                          "/Platform/RaspberryPi/RPi4/RPi4.dsc"))))
            (replace 'install
              (lambda _
                (let ((firmware (string-append #$output "/share/firmware")))
                  (mkdir-p firmware)
                  (install-file "Build/RPi4/RELEASE_GCC/FV/RPI_EFI.fd"
                                firmware))))
            (add-before 'install 'install-efi-shell
              (lambda _
                (let ((fmw (string-append #$output "/share/firmware")))
                  (mkdir-p fmw)
                  (for-each
                    (lambda (file)
                      (copy-file file
                                 (string-append fmw "/Shell_"
                                                (string-downcase #$arch-string)
                                                ".efi")))
                    (find-files "Build" "Shell\\.efi")))))
            (add-after 'install 'install-config.txt
              (lambda _
                (call-with-output-file
                  (string-append #$output "/share/firmware/config.txt")
                  (lambda (port)
                    (format port "\
arm_64bit=1
arm_boost=1
enable_uart=1
uart_2ndstage=1
enable_gic=1
armstub=RPI_EFI.fd
disable_commandline_tags=1
disable_overscan=1
device_tree_address=0x1f0000
device_tree_end=0x200000~%"))))))))
      (native-inputs
       `(("acpica" ,acpica)
         ("nasm" ,nasm)
         ("perl" ,perl)
         ("python" ,python-3)
         ("util-linux:lib" ,util-linux "lib")
         ("edk2-platforms"
          ,(let ((revision "0")
                 (commit "728c8bb974be69b4034fad7a1c60917cca2dd03d"))
             (origin
               (method git-fetch)
               (uri (git-reference
                      (url "https://github.com/tianocore/edk2-platforms")
                      (commit commit)))
               (file-name
                (git-file-name "edk2-platforms"
                               (git-version "0.0.0" revision commit)))
               (sha256
                (base32
                 "1z1kdl6lsfrj151pnhqrfzs8xfwcgysyfq23kc5d93cc0vr2znm0")))))
         ("edk2-rpi4-osi" ,edk2-rpi4-osi)
         ,@(if (not (string-prefix? "aarch64" (%current-system)))
               `(("cross-gcc" ,(cross-gcc "aarch64-linux-gnu"))
                 ("cross-binutils" ,(cross-binutils "aarch64-linux-gnu")))
                '())))
      (synopsis "UEFI firmware for Raspberry Pi 4")
      (description "This package provides a Raspberry Pi 4 UEFI firmware
based on EDK II.")
      (license (list license:expat
                     license:bsd-2 license:bsd-3 license:bsd-4)))))
