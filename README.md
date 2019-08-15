# securekilo
A terminal text editor that encrypts files with gpg.

It is based on the version of [antirez's kilo](https://github.com/antirez/kilo "Original kilo editor") that is explained step by step in [snaptoken's tutorial](https://viewsourcecode.org/snaptoken/kilo/).

Text files are stored as symmetrically encrypted data (AES cipher) obtained by piping the clear data into the input of `gpg -c`. When opening a file, the encrypted data is piped into the input of `gpg -d`. As a consequence, when opening/saving a file, the user is asked by gpg for a passphrase to decrypt/encrypt the content. The goal is to avoid having the clear data in a file at any time.
