After setting up nonguix, run the following:
```
sudo guix archive --authorize < signing-key.pub
sudo guix system reconfigure /etc/config.scm --substitute-urls='https://ci.guix.gnu.org https://bordeaux.guix.gnu.org https://substitutes.nonguix.org'
```
