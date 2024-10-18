;;; SPDX-FileCopyrightText: © 2024 Jean-Pierre De Jesus DIAZ <me@jeandudey.tech>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (raspberry packages firmware)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages assembly)
  #:use-module (gnu packages cross-base)
  #:use-module (gnu packages firmware)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages python)
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

(define-public edk2-rpi4
  (let ((toolchain "GCC")
        (arch-string "AARCH64"))
    (package
      (inherit edk2-tools)
      (name "edk2-rpi4")
      (version "202408")
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
                  "0806470j5rdn2l9mngc5yriy0gas3017gx74frqkwr40m175m1ys"))))
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
                            #$(this-package-native-input "edk2-non-osi")))
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
                    (find-files "Build" "Shell\\.efi"))))))))
      (native-inputs
       `(("acpica" ,acpica)
         ("nasm" ,nasm)
         ("perl" ,perl)
         ("python" ,python-3)
         ("util-linux:lib" ,util-linux "lib")
         ("edk2-platforms"
          ,(let ((revision "0")
                 (commit "6146fd7abcf6bfa083d33d1dfdc750694fc040a7"))
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
                 "14dq3j3bpg4s5vjxszz2nhdfqc0094fxan3amd9ssayb8v7rdg98")))))
         ("edk2-non-osi"
          ,(let ((revision "0")
                 (commit "0544808c623bb73252310b1e5ef887caaf08c34b"))
             (origin
               (method git-fetch)
               (uri (git-reference
                      (url "https://github.com/tianocore/edk2-non-osi")
                      (commit commit)))
               (file-name
                (git-file-name "edk2-non-osi"
                               (git-version "0.0.0" revision commit)))
               (modules '((guix build utils)))
               (snippet
                #~(begin
                    (for-each delete-file (find-files "." "\\.(exe|bin)$"))
                    (copy-file #$(file-append arm-trusted-firmware-rpi4 "/bl31.bin")
                               "Platform/RaspberryPi/RPi4/TrustedFirmware/bl31.bin")))
               (sha256
                (base32
                 "037py14vfdxf8cv16fmim8s1c0fyagwysw07qv5slks7pjkzbl6k")))))
         ,@(if (not (string-prefix? "aarch64" (%current-system)))
               `(("cross-gcc" ,(cross-gcc "aarch64-linux-gnu"))
                 ("cross-binutils" ,(cross-binutils "aarch64-linux-gnu")))
                '())))
      (synopsis "UEFI firmware for Raspberry Pi 4")
      (description "This package provides a Raspberry Pi 4 UEFI firmware
based on EDK II.")
      (license (list license:expat
                     license:bsd-2 license:bsd-3 license:bsd-4)))))
