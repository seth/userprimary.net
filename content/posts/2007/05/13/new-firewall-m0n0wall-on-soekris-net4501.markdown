To downsize our home compute farm, I purchased a <a href="http://www.soekris.com/net4501.htm">net4501</a> from <a href="http://www.soekris.com/index.htm">SOEKRIS Engineering</a> to replace an old tower box that we had been using as our firewall.  I've been using <a href="http://m0n0.ch/wall">m0n0wall</a> as a home firewall for some years now and have been really happy with it.  All configuration is done using a web interface and it provides many sophisticated features.  For example, it is easy to setup a captive portal for your home wifi so that you can have an open access point that requires a username and password to login.  This is useful because most other solutions lead to technical challenges when friends want to use your network.

Here are some notes on how I set things up using OS X.


<span style="font-size:15pt;"><strong>m0n0wall on a Soekris net4501 using OS X</strong></span>



<strong>Writing the Flash Card</strong>


First plug in a USB compact flash card reader.  Go to the apple menu and select 'About This Mac'.  Click on 'More Info...' and look at the USB details to discover the BSD disk name.  For me it was /dev/disk2.  That's a lie, at first it was /dev/disk2e1 or something like that, but that probably referred to the partition and you want the disk.

Next unmount.  The mount point is the name of the flash card in Finder and it appears in /Volumes/&lt;name&gt;.  I used:

<code>
sudo umount "/Volumes/NO NAME"</code>

Next, write the m0n0wall image file to the card:

<code>
gunzip -c ~/Downloads/net45xx-1.231.img | sudo dd of=/dev/disk2 bs=16k</code>

Eject the USB reader and give it a whirl.  You may need to repeat this process.  The second time, be sure to check for the BSD name as it may change.  You won't have to repeat the umount step because after the first dd call you won't have a flash card that will get automatically mounted.


<strong>Connecting to the Soekris box</strong>


Put the flash card in into the slot in the Soekris box and plug in the power cable.  Connect a network cable from port closest to the serial port to your computer.  Be sure your computer is setup for DHCP and wait 3-5 minutes.  If you don't get an IP address like 192.168.1.xxx, then try repeating the writing the flash card step.  If you do get an IP address from the Soekris box, then open a browser and go to http://192.168.1.1 and follow <a href="http://doc.m0n0.ch/quickstartsoekris/">the m0n0wall documentation</a> to configure your new firewall.
