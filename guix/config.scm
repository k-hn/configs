;; This is an operating system configuration generated
;; by the graphical installer.
;;
;; Once installation is complete, you can learn and modify
;; this file to tweak the system configuration, and pass it
;; to the 'guix system reconfigure' command to effect your
;; changes.

;; Indicate which modules to import to access the variables
;; used in this configuration.
(use-modules (gnu))
(use-modules (gnu services pm))
(use-modules (nongnu packages linux)
            (nongnu system linux-initrd))
(use-service-modules cups desktop networking ssh xorg)

;; Non-guix service
(define %modified-desktop-services
  (modify-services %desktop-services
		   (guix-service-type config =>
				      (guix-configuration
				       (inherit config)
				       (substitute-urls
					(append (list "https://substitutes.nonguix.org")
						%default-substitute-urls))
				       (authorized-keys
					(append (list (local-file "./nonguix-signing-key.pub"))
						%default-authorized-guix-keys))))
		   (gdm-service-type config =>
				     (gdm-configuration
				      (inherit config)
				      (wayland? #t)))))

(operating-system
  (kernel linux)
  (kernel-arguments '("mem_sleep_default=deep"))
  (initrd microcode-initrd)
  (firmware (list linux-firmware))
  (locale "en_US.utf8")
  (timezone "Africa/Accra")
  (keyboard-layout (keyboard-layout "us"))
  (host-name "slate")

  ;; The list of user accounts ('root' is implicit).
  (users (cons* (user-account
                  (name "hil")
                  (comment "Hilary")
                  (group "users")
                  (home-directory "/home/hil")
                  (supplementary-groups '("wheel" "netdev" "audio" "video")))
                %base-user-accounts))

  ;; Packages installed system-wide.  Users can also install packages
  ;; under their own account: use 'guix search KEYWORD' to search
  ;; for packages and 'guix install PACKAGE' to install a package.
  ;; (packages (append (list (specification->package "nss-certs")
  ;; 			  (specification->package "git")
  ;; 			  (specification->package "font-microsoft-web-core-fonts")
  ;; 			  (specification->package "trash-cli")
  ;; 			  (specification->package "flatpak")
  ;; 			  (specification->package "xdg-desktop-portal-gtk"))
  ;;                   %base-packages))
  (packages (append (map specification->package '("nss-certs"
						  "git"
						  "font-microsoft-web-core-fonts"
						  "trash-cli"
						  "flatpak"
						  "xdg-desktop-portal-gtk"
						  "powertop"))
                    %base-packages))

  ;; Below is the list of system services.  To search for available
  ;; services, run 'guix system search KEYWORD' in a terminal.
  (services
   (append (list (service gnome-desktop-service-type)

		 ;; power management
		 (service tlp-service-type
			  (tlp-configuration
			   (cpu-scaling-governor-on-ac (list "performance"))
			   (sched-powersave-on-bat? #t)))
		 (service thermald-service-type)
		 
                 ;; To configure OpenSSH, pass an 'openssh-configuration'
                 ;; record as a second argument to 'service' below.
                 (service openssh-service-type)
                 (service cups-service-type)
                 (set-xorg-configuration
                  (xorg-configuration (keyboard-layout keyboard-layout))))

           ;; This is the default list of services we
           ;; are appending to.
	   ;; instead of %desktop-services, %nonguix-service modifies and returns %desktop-service
	   %modified-desktop-services))
  (bootloader (bootloader-configuration
                (bootloader grub-efi-bootloader)
                (targets (list "/boot/efi"))
                (keyboard-layout keyboard-layout)))
  (initrd-modules (append '("vmd") %base-initrd-modules))
  (swap-devices (list (swap-space
                        (target (uuid
                                 "2b714bd8-dbdb-456c-8a3b-4d47cc224466")))))

  ;; The list of file systems that get "mounted".  The unique
  ;; file system identifiers there ("UUIDs") can be obtained
  ;; by running 'blkid' in a terminal.
  (file-systems (cons* (file-system
                         (mount-point "/")
                         (device (uuid
                                  "e26fa27e-1e71-485a-8046-101fea50888e"
                                  'btrfs))
                         (type "btrfs"))
                       (file-system
                         (mount-point "/boot/efi")
                         (device (uuid "D272-A237"
                                       'fat32))
                         (type "vfat")) %base-file-systems)))
